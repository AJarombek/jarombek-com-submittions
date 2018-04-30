import React from "react";

/**
 * Link React Component
 * @author Andrew Jarombek
 * @since 4/29/2018
 */

const Link = ({ name, link }) => 
    <a href={link}>
      {name}
    </a>;

export default Link;