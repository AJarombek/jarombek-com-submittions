/**
 * Main application component for the React 16.3 demo.  Contains a bunch of cards that can be
 * selected to learn more about a React 16.3 feature.
 * @author Andrew Jarombek
 * @since 11/14/2019
 */

import { hot } from 'react-hot-loader/root';
import React from 'react';
import { useHistory } from 'react-router-dom';
import { AJTextCard, AJResponsiveGrid } from 'jarombek-react-components';

const App = () => {
  const history = useHistory();

  const contextNode =
    <>
      In React 16.3, the Context API was rewritten. Context allows data to be shared amongst
      components without passing it via props from parents to children<sup>1</sup>. While I'm
      still learning which scenarios are appropriate for the context API, the React blog
      recommends to use it for "global" data used by a tree of React components<sup>2</sup>.
    </>;

  const createRefNode =
    <>
      Refs are a feature I've known about and used since I first learned React.  Refs provide a way
      to access a DOM node and use the DOM API.  React is well known for its virtual DOM, a layer of
      abstraction on top of the DOM.  Usually we interact with the virtual DOM in a declarative
      way, however Refs allow us to write traditional imperative DOM API code.  React 16.3
      introduced a new way to create Refs with the <code>createRef()</code> method.
    </>;

  return (
    <div className="App">
      <h1>React 16.3</h1>
      <p>Release Date: March 29, 2018</p>
      <AJResponsiveGrid
        smallBreakpoint="500px"
        mediumBreakpoint="900px"
        largeBreakpoint="1200px"
        items={[
          <AJTextCard
            key="context"
            title="Context"
            content={contextNode}
            action={() => history.push('/context')}
            actionText="Learn More"
          />,
          <AJTextCard
            key="create-ref"
            title="Create Ref"
            content={createRefNode}
            action={() => history.push('/create-ref')}
            actionText="Learn More"
          />,
          <AJTextCard
            key="forward-ref"
            title="Forward Ref"
            content={<>...</>}
            action={() => history.push('/forward-ref')}
            actionText="Learn More"
          />,
          <AJTextCard
            key="new-lifecycle"
            title="New Lifecycle"
            content={<>...</>}
            action={() => history.push('/new-lifecycle')}
            actionText="Learn More"
          />,
          <AJTextCard
            key="strict-mode"
            title="Strict Mode"
            content={<>...</>}
            action={() => history.push('/strict-mode')}
            actionText="Learn More"
          />
        ]}
      />
    </div>
  );
};

export default hot(App);
