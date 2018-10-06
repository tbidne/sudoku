import * as React from 'react';
import '../styles/Cell.css';

interface ICell {
    cellId: number;
    row: number;
    col: number;
    realValue: number;
    userValue: number;
    revealed: boolean;
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
                id="cell-{this.props.id}"
                name="cell-{this.props.id}"
                min={1}
                max={9}
                height="md"
                width="md"
                value={this.getShownValue()}
                disabled={this.props.revealed}
            />
        );
    }

    private getClassName(): string {
        return this.props.revealed ? 'cell revealed' : 'cell ';
    }

    private getShownValue(): any {
        if (this.props.revealed) {
            return this.props.realValue;
        }
        return this.props.userValue > 0 ? this.props.userValue : null;
    }
}