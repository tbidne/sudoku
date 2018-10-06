import * as React from 'react';
import Grid from './components/Grid';
import haskell from './haskell.svg';
import logo from './logo.svg';
import './styles/App.css';
import ts from './ts.png';

class App extends React.Component {
    public render() {
        return (
        <div className="App">
            <header className="App-header">
            <h1 className="App-title">Powered by</h1>
            <img src={ts} className="logo" alt="logo" />
            <img src={logo} className="Rotating-logo" alt="logo" />
            <img src={haskell} className="logo" alt="logo" />
            </header>
            <Grid/>
        </div>
        );
    }
}

export default App;
