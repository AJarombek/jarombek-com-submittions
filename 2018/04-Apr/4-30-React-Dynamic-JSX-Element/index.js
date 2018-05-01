import React from "react";
import { render } from "react-dom";
import Link from "./Link";

/**
 * Demonstrate creating dynamic JSX elements
 * @author Andrew Jarombek
 * @since 4/29/2018
 */

const elements = [
  {
    tag: "h1",
    attributes: null,
    value: "Webpage Title"
  },
  {
    tag: "p",
    attributes: null,
    value: "Hey"
  },
  {
    tag: "Link",
    attributes: {
      name: "Check out my GitHub!",
      link: "https://github.com/AJarombek"
    },
    value: "Ok"
  }
];

const App = () =>
  elements.map(e => {
    let Tag = e.tag;

    // If we are using a custom react component, we must
    // assign it directly.
    if (Tag === "Link") {
      Tag = Link;
    }

    return (
      <Tag key={e.toString()} {...e.attributes}>
        {e.value}
      </Tag>
    );
  });

render(<App />, document.getElementById("root"));
