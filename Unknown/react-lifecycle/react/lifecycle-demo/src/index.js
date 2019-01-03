/**
 * Entry point for React into the HTML document
 * @author Andrew Jarombek
 * @since 1/3/2019
 */

import React from 'react';
import ReactDOM from 'react-dom';
import App from './App';
import registerServiceWorker from './registerServiceWorker';

ReactDOM.render(<App />, document.getElementById('root'));
registerServiceWorker();
