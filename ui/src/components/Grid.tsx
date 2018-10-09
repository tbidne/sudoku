import * as React from 'react';
import { ReactElement, SyntheticEvent } from 'react';
import { CellDto } from '../domain/cell.dto';
import { GridDto } from '../domain/grid.dto';
import { RestService } from '../services/rest.service';
import '../styles/Grid.css';
import Cell from './Cell';

interface IGridState {
    message: string,
    grid: GridDto,
    selectedCell: number
}

export default class Grid extends React.Component<{}, IGridState> {

    private restService: RestService;

    constructor(props: any) {
        super(props);

        this.state = { message: '', grid: new GridDto(), selectedCell: -1 };

        this.clearGrid = this.clearGrid.bind(this);

        this.restService = new RestService();
        this.restService.health().then(response => {
            this.setState({ message: response });
        });
        this.restService.getGrid(0).then(response => {
            this.setState({ grid: response });
        });
    }

    public renderCell(cellId: number, row: number, col: number,
        realValue: number, userValue: number, revealed: boolean): JSX.Element {

        return <Cell cellId = {cellId} row={row} col={col} realValue={realValue}
        userValue={userValue} revealed={revealed} key={cellId}
        onChange={this.updateCell} onSelect={this.selectCell}/>;
    }

    public render() {
        if (!this.state.grid || !this.state.grid.cells) {
            return (<div/>);
        }

        const divStyle = {
            fontSize: '25px'
        };

        const rows: JSX.Element[] = this.renderCells(this.state.grid.cells);
        return (
            <div style={divStyle}>
                {this.state.message}
                <div>
                    {this.renderInstructions()}
                    <div className="middle">
                        {rows}
                    </div>
                    {this.renderButtons()}
                </div>
            </div>
        );
    }

    private renderCells(cells: CellDto[]): JSX.Element[] {
        let i = 0;
        const rows: JSX.Element[] = [];
        const sorted = this.sortCells(cells);
        
        for (let r = 0 ; r < 9 ; r++) {
            const cols: JSX.Element[] = [];
            for (let c = 0 ; c < 9 ; c++) {
                const cell = sorted[i];
                cols.push(this.renderCell(cell.cellId, cell.row, cell.col, cell.realValue, cell.userValue, cell.revealed));
                i++;
            }
            rows.push(<div className="grid-row" key={r}>{cols}</div>);
        }
        return rows;
    }

    private sortCells(cells: CellDto[]): CellDto[] {
        return cells.sort((c: CellDto, d: CellDto): number => {
            return c.row !== d.row ? c.row - d.row : c.col - d.col;
        });
    }

    private renderInstructions(): ReactElement<HTMLDivElement> {
        const fontStyle = {
            fontSize: '15px'
        };

        return <div style={fontStyle} className="left">
                <h4>How to use:</h4>
                <ol className="blah">
                    <li>Start a new game with <strong>clear</strong>.</li>
                    <li>Fill in blanks with initial values; then click <strong>solve</strong>. This can take a few minutes.</li>
                    <li>Continue filling out the puzzle normally. You can <strong>save</strong> at any time.</li>
                    <li><strong>Reveal cell</strong> will reveal the currently selected cell.</li>
                    <li><strong>Reveal puzzle</strong> will reveal the entire puzzle.</li>
                    <li>Have fun!</li>
                </ol>
            </div>;
    }

    private clearGrid(): void {
        this.restService.clear(0).then(response => {
            this.setState({ grid: response });
        });
    }

    private renderButtons(): ReactElement<HTMLDivElement> {
        const style = {
            fontSize: '20px'
        };

        return <div className="right" style={style}>
                <button className="btn btn-danger" onClick={this.clearGrid}>Clear</button><br/>
                <button className="btn btn-secondary">Save</button><br/>
                <button className="btn btn-secondary" disabled={this.state.grid.solved}>Solve</button><br/>
                <button className="btn btn-primary">Reveal Cell</button><br/>
                <button className="btn btn-success">Reveal Puzzle</button>
            </div>;
    }

    private updateCell = (event: any) => {
        const x = event.target.value;
    }

    private selectCell = (event: any) => {
        const y = event.target.value;
    }
}