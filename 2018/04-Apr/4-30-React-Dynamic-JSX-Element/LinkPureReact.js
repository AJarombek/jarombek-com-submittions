import React from "react";

/**
 * Primitive version of the Link component using React.createElement()
 * @author Andrew Jarombek
 * @since 4/29/2018
 */

const Link = ({ name, link }) => 
    React.createElement("a", {href: link}, name);

export default Link;