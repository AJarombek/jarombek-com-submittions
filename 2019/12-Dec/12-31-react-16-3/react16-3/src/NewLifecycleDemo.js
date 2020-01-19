/**
 * Describe the new lifecycle methods introduced in React 16.3.
 * @author Andrew Jarombek
 * @since 1/17/2020
 */

import React from 'react';
import FeaturePage from './FeaturePage';
import { AJCodeSnippet } from 'jarombek-react-components';

const NewLifecycleDemo = () => {
    return (
        <FeaturePage>
            <h1>New Lifecycle Methods</h1>
          <div className="demo-body">
            <p>
              Last year I wrote an article about <a
              href="https://jarombek.com/blog/jan-19-2019-react-lifecycles">lifecycle
              methods in React</a>.  It turns out my article was outdated upon arrival, since the
              React team began a multi-year process of deprecating existing lifecycles and adding
              new ones.  In React 16.3, two new lifecycle methods were introduced -
              <code>getDerivedStateFromProps()</code> and <code>getSnapshotBeforeUpdate()</code>.
            </p>
            <p>
              It was also announced that three lifecycle methods will be slowly phased out over the
              coming releases.  In React 17 (still not released at the time I'm writing this - Jan.
              17th, 2020) <code>componentWillMount</code>, <code>componentWillReceiveProps</code>,
              and <code>componentWillUpdate</code> will be removed.  Legacy code can simply rename
              these methods to <code>UNSAFE_componentWillMount</code>,
              <code>UNSAFE_componentWillReceiveProps</code>, and
              <code>UNSAFE_componentWillUpdate</code> instead of being refactored.  These
              <code>UNSAFE</code> prefixed methods were introduced in React 16.3 in case developers
              want to rename existing methods before React 17 is released.
            </p>
            <p>
              Once React 17 is released, the class component lifecycles will be:
            </p>
            <p>
              <code className="code-listing">constructor()</code> (Included although technically not
              a lifecycle method)
            </p>
            <p>
              <code className="code-listing">componentDidMount()</code>
            </p>
            <p>
              <code className="code-listing">render()</code>
            </p>
            <p>
              <code className="code-listing">shouldComponentUpdate()</code>
            </p>
            <p>
              <code className="code-listing">getDerivedStateFromProps()</code>
            </p>
            <p>
              <code className="code-listing">getSnapshotBeforeUpdate()</code>
            </p>
            <p>
              <code className="code-listing">componentDidUpdate()</code>
            </p>
            <p>
              <code className="code-listing">componentWillUnmount()</code>
            </p>
          </div>
        </FeaturePage>
    );
};

export default NewLifecycleDemo;
