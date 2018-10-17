import Axios from 'axios-observable';
import { AxiosObservable } from 'axios-observable/dist/axios-observable.interface';
import { CellDto } from '../domain/cell.dto';
import { GridDto } from '../domain/grid.dto';

export class RestService {

    public health(): AxiosObservable<string> {
        return Axios.get<string>('http://localhost:3001/health');
    }

    public getGrid(id: number): AxiosObservable<GridDto> {
        return Axios.get<GridDto>(`http://localhost:3001/grid/${id}`);
    }

    public clear(id: number): AxiosObservable<GridDto> {
        return Axios.put<GridDto>(`http://localhost:3001/grid/${id}/clear`);
    }

    public save(id: number, grid: GridDto): AxiosObservable<GridDto> {
        return Axios.put<GridDto>(`http://localhost:3001/grid/${id}`, grid);
    }

    public solve(id: number, grid: GridDto): AxiosObservable<GridDto> {
        return Axios.put<GridDto>(`http://localhost:3001/grid/${id}/solve`, grid);
    }

    public revealCell(id: number, cell: CellDto): AxiosObservable<CellDto> {
        return Axios.put<CellDto>(`http://localhost:3001/cell/${id}/reveal`, cell);
    }

    public revealAll(id: number, grid: GridDto): AxiosObservable<GridDto> {
        return Axios.put<GridDto>(`http://localhost:3001/grid/${id}/reveal`, grid);
    }
}