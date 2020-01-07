/**
 * Implement a global theme for components using the context API.  This is in contrast with
 * {@link ThemeWithProps.js}, which uses props.
 * @author Andrew Jarombek
 * @since 1/2/2020
 */

import React, { createContext, useContext, useState } from 'react';
import { useHistory } from 'react-router-dom';
import { AJNavTextCircle } from 'jarombek-react-components';
import classnames from 'classnames';

const ThemeContext = React.createContext('light');

const ThemeWithContext = () => {
  const [theme, setTheme] = useState('light');

  return (
    <div className="theme-with-context">
      <ThemeContext.Provider value={theme}>
        <AJSwitchIcon
          initialState={true}
          disabled={false}
          onChange={() => setTheme(theme === 'dark' ? 'light' : 'dark')}
          offImageUrl="assets/green.png"
          onImageUrl="assets/light.png"
        />
        <ThemeWithContext />
      </ThemeContext.Provider>
    </div>
  );
};

const ThemeWithContextCard = () => {
  const context = useContext(ThemeContext);

  return (
    <div className={classnames('theme-with-context-card', `${context}-theme-with-context-card`)}>
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

export default ThemeWithContext;
