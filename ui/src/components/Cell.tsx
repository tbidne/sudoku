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
    onBlur: any;
}

export default class Cell extends React.Component<ICell, {}> {
    constructor(props: any) {
        super(props);
    }

    public render() {
        return (
            <input style={{height: 40, width: 40, textAlign: 'center'}}
                className={this.getClassName()}
                type="text"
                id={this.getId()}
                name={this.getId()}
                placeholder=""
                maxLength={1}
                value={this.getShownValue()}
                disabled={this.props.revealed}
                onChange={this.props.onChange(this.props.cellId)}
                onSelect={this.props.onSelect(this.props.cellId)}
                onBlur={this.props.onBlur}
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
        return this.props.userValue > 0 ? this.props.userValue : '';
    }
}