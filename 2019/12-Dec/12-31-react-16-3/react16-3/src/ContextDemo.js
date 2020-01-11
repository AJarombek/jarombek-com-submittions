/**
 * Describe the context API introduced in React 16.3.
 * @author Andrew Jarombek
 * @since 12/30/2019
 */

import React from 'react';
import { useHistory } from 'react-router-dom';
import { AJNavTextCircle, AJCodeSnippet } from 'jarombek-react-components';
import FeaturePage from './FeaturePage';
import ThemeWithProps from './ThemeWithProps';
import ThemeWithContext from './ThemeWithContext';

const themeWithPropsCode = `// ThemeWithProps.js

import React, {useState} from 'react';
import {useHistory} from 'react-router-dom';
import {AJTextCard, AJSwitchIcon} from 'jarombek-react-components';
import classnames from 'classnames';

import green from './assets/green.png';
import light from './assets/light.png';

const ThemeWithProps = () => {
  const [theme, setTheme] = useState('light');

  return (
    <div className={classnames("theme-with-props", \`\${theme}-theme-with-props\`)}>
      <AJSwitchIcon
        initialState={true}
        disabled={false}
        onChange={() => setTheme(theme === 'dark' ? 'light' : 'dark')}
        offImageUrl={green}
        onImageUrl={light}
      />
      <ThemeWithPropsCard theme={theme} />
    </div>
  );
};

const ThemeWithPropsCard = ({theme}) => {
  return (
    <div className={classnames('theme-with-props-card', \`\${theme}-theme-with-props-card\`)}>
      <AJTextCard
        title="Theme implemented with props"
        content="... content ..."
        action={null}
        actionText="Button"
        actionDisabled={false}
      />
    </div>
  );
};
`;

const themeWithContextCode = `// ThemeWithContext.js

import React, { createContext, useContext, useState } from 'react';
import { useHistory } from 'react-router-dom';
import { AJSwitchIcon, AJTextCard } from 'jarombek-react-components';
import classnames from 'classnames';

import green from './assets/green.png';
import light from './assets/light.png';

const ThemeContext = createContext('light');

const ThemeWithContext = () => {
  const [theme, setTheme] = useState('light');

  return (
    <div className={classnames("theme-with-context", \`\${theme}-theme-with-context\`)}>
      <ThemeContext.Provider value={theme}>
        <AJSwitchIcon
          initialState={true}
          disabled={false}
          onChange={() => setTheme(theme === 'dark' ? 'light' : 'dark')}
          offImageUrl={green}
          onImageUrl={light}
        />
        <ThemeWithContextCard />
      </ThemeContext.Provider>
    </div>
  );
};

const ThemeWithContextCard = () => {
  const context = useContext(ThemeContext);

  return (
    <div className={classnames('theme-with-context-card', \`\${context}-theme-with-context-card\`)}>
      <AJTextCard
        title="Theme implemented with the context API"
        content="... content ..."
        action={null}
        actionText="Button"
        actionDisabled={false}
      />
    </div>
  );
};
`;

const ContextDemo = () => {
  return (
    <FeaturePage>
      <h1>Context API</h1>
      <div className="context-demo-body">
        <p>
          In React 16.3, the Context API was rewritten. Context allows data to be shared
          amongst components without passing it via props from parents to children<sup>1</sup>.
          While I'm still learning which scenarios are appropriate for the context API, the React
          blog recommends to use it for "global" data used by a tree of React
          components<sup>2</sup>.
        </p>
        <p>
          One good use case for the context API is setting the stylistic theme of a web page based
          on a toggleable switch.  All the components on a page should be aware of the theme chosen,
          but ideally it doesn't need to be passed to every component via a prop.
        </p>
        <p>
          First I implemented this scenario using props.
        </p>
        <ThemeWithProps />
        <p>
          Next I implemented the same component with the context API.
        </p>
        <ThemeWithContext />
        <p>
          Both components are functionally equivalent.  To identify the differences between the two
          components, let's take a look at the the code.
        </p>
        <AJCodeSnippet language="javascript">
          {themeWithPropsCode}
        </AJCodeSnippet>
        <AJCodeSnippet language="javascript">
          {themeWithContextCode}
        </AJCodeSnippet>
      </div>
    </FeaturePage>
  );
};

export default ContextDemo;
