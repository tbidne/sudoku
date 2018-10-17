import axios from 'axios';
import * as React from 'react';
import { ReactElement } from 'react';
import { Subscription } from 'rxjs';
import { CellDto } from '../domain/cell.dto';
import { GridDto } from '../domain/grid.dto';
import { RestService } from '../services/rest.service';
import '../styles/Grid.css';
import Cell from './Cell';

interface IGridState {
    message: string,
    grid: GridDto,
    selectedCellId: number
}

export default class Grid extends React.Component<{}, IGridState> {

    private restService: RestService;
    private subscriptions: Subscription[] = [];

    constructor(props: any) {
        super(props);

        this.state = { message: '', grid: new GridDto(), selectedCellId: -1 };

        this.clearGrid = this.clearGrid.bind(this);
        this.save = this.save.bind(this);
        this.solve = this.solve.bind(this);
        this.revealCell = this.revealCell.bind(this);
        this.revealAll = this.revealAll.bind(this);

        this.restService = new RestService();
        this.subscriptions.push(this.restService.health().subscribe(response => {
            this.setState({ message: response.data });
        }));
        this.subscriptions.push(this.restService.getGrid(0).subscribe(response => {
            this.setState({ grid: response.data });
        }));
    }

    public renderCell(cellId: number, row: number, col: number,
        realValue: number, userValue: number, revealed: boolean): JSX.Element {

        return <Cell cellId = {cellId} row={row} col={col} realValue={realValue}
        userValue={userValue} revealed={revealed} key={cellId}
        onChange={this.updateCell} onSelect={this.selectCell} onBlur={this.deselectCell}/>;
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

    public componentWillUnmount() {
        this.subscriptions.forEach(sub => sub.unsubscribe());
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
        this.subscriptions.push(this.restService.clear(0).subscribe(response => {
            this.setState({ grid: response.data });
        }));
    }

    private save(): void {
        this.restService.save(0, this.state.grid);
    }

    private solve(): void {
        const grid = this.state.grid;
        grid.cells.forEach(c => {
            c.realValue = c.userValue;
        });
        this.subscriptions.push(this.restService.solve(0, grid).subscribe(response => {
            this.setState({ grid: response.data });
        }));
    }

    private revealCell(): void {
        const cell = this.state.grid.cells.find(c => c.cellId === this.state.selectedCellId);
        if (!cell) {
            return;
        }

        this.subscriptions.push(this.restService.revealCell(cell.cellId, cell).subscribe(response => {
            const newGrid = this.state.grid;
            const newCells = newGrid.cells.filter(c => c.cellId !== response.data.cellId);
            newCells.push(response.data);
            newGrid.cells = newCells;
            this.setState({ grid: newGrid });
        }));
    }

    private revealAll(): void {
        this.subscriptions.push(this.restService.revealAll(0, this.state.grid).subscribe(response => {
            this.setState({ grid: response.data });
        }));
    }

    private renderButtons(): ReactElement<HTMLDivElement> {
        const style = {
            fontSize: '20px'
        };

        return <div className="right" style={style}>
                <button className="btn btn-danger"
                    onClick={this.clearGrid}>
                    Clear
                </button><br/>
                <button className="btn btn-secondary"
                    onClick={this.solve}
                    disabled={this.state.grid.solved}>
                    Solve
                </button><br/>
                <button className="btn btn-secondary"
                    onClick={this.save}
                    disabled={!this.state.grid.solved || this.allRevealed()}>
                    Save
                </button><br/>
                <button className="btn btn-primary" /* mousedown needed because blur */
                    onMouseDown={this.revealCell}   /* and disabled disrupts click   */
                    disabled={!this.canRevealCell()}>
                    Reveal Cell
                </button><br/>
                <button className="btn btn-success"
                    onClick={this.revealAll}
                    disabled={!this.state.grid.solved || this.allRevealed()}>
                    Reveal Puzzle
                </button>
            </div>;
    }

    private allRevealed(): boolean {
        return this.state.grid.cells.every(c => c.revealed);
    }

    private updateCell = (id: number) => (event: any) => {
        const cell = this.state.grid.cells.filter(c => c.cellId === id)[0];
        cell.userValue = +event.target.value;
        this.setState({ grid: this.state.grid });
        this.renderCell(cell.cellId, cell.row, cell.col, cell.realValue, cell.userValue, cell.revealed);
    }

    private selectCell = (id: number) => (event: any) => {
        this.setState({ selectedCellId: id });
    }

    private deselectCell = (event: any) => {
        this.setState({ selectedCellId: -1 });
    }

    private canRevealCell(): boolean {
        const selected = this.state.grid.cells.find(c => c.cellId === this.state.selectedCellId);
        const selectedAndNotRevealed = selected ? !selected.revealed : false;
        return this.state.grid.solved && selectedAndNotRevealed;
    }
}