import React, { Component } from "react";
import logo from "./logo.svg";
import {
  helloWorld,
  helloWorld2,
  helloWorld4,
  helloWorld6,
  helloWorld9,
  helloWorld8
} from "./ps/output/HelloWorld";
import "./App.css";

class App extends Component {
  render() {
    return (
      <div className="App">
        <header className="App-header">
          <img src={logo} className="App-logo" alt="logo" />
          <h1 className="App-title">Welcome to React</h1>
        </header>
        <p className="App-intro">
          To get started, edit <code>src/App.js</code> and save to reload.
        </p>
        <p>ja</p>
        {helloWorld()}
        {helloWorld2}
        {helloWorld4}
        {helloWorld6}
        {helloWorld8}
        {helloWorld9({
          name: "world",
          routeToUrl: a => b => () => console.log(a)
        })}
      </div>
    );
  }
}

export default App;
