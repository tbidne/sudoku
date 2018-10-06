export class CellDto {
  public cellId: number;
  public row: number;
  public col: number;
  public realValue: number;
  public userValue: number;
  public revealed: boolean;

  constructor(init?: any) {
    if (init) {
      this.cellId = init.cellId;
      this.row = init.row;
      this.col = init.col;
      this.realValue = init.realValue;
      this.userValue = init.userValue;
      this.revealed = init.revealed;
    }
  }
}