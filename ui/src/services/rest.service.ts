import axios from 'axios';
import { GridDto } from '../domain/grid.dto';

export class RestService {

    public async health(): Promise<string> {
        try {
            const response = await axios.get<string>('http://localhost:3001/health');
            return response.status === 200 ? response.data : `/health returned http ${response.status}`;
        } catch (e) {
            return `Something went wrong: ${e.message}`;
        }
    }

    public async getGrid(id: number): Promise<GridDto> {
        const response = await axios.get<GridDto>(`http://localhost:3001/grid/${id}`);
        return response.data;
    }

    public async clear(id: number): Promise<GridDto> {
        const response = await axios.put<GridDto>(`http://localhost:3001/grid/${id}/clear`);
        return response.data;
    }
}