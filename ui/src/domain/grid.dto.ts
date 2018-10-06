import { CellDto } from './cell.dto';

export class GridDto {
  public gridId: number;
  public cells: CellDto[];
  public solved: boolean;

  public constructor(init?: any) {
    if (init) {
      this.gridId = init.gridId;
      this.cells = init.cells;
      this.solved = init.solved;
    }
  }
}