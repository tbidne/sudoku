import * as React from 'react';
import '../styles/Cell.css';

interface ICell {
    cellId: number;
    row: number;
    col: number;
    realValue: number;
    userValue: number;
    revealed: boolean;
    onChange: any;
    onSelect: any;
}

export default class Cell extends React.Component<ICell, {}> {
    constructor(props: any) {
        super(props);
    }

    public render() {
        return (
            <input
                className={this.getClassName()}
                type="number"
                id={this.getId()}
                name={this.getId()}
                min={1}
                max={9}
                height="md"
                width="md"
                placeholder=""
                value={this.getShownValue()}
                disabled={this.props.revealed}
                onChange={this.props.onChange(this.props.cellId)}
                onSelect={this.props.onSelect}
            />
        );
    }

    private getId(): string {
        return `cell-${this.props.cellId}`
    }

    private getClassName(): string {
        return this.props.revealed ? 'cell revealed' : 'cell ';
    }

    private getShownValue(): any {
        if (this.props.revealed) {
            return this.props.realValue;
        }
        return this.props.userValue > 0 ? this.props.userValue : undefined;
    }
}