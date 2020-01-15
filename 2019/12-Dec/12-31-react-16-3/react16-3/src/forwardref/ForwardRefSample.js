/**
 * Demonstrate how forward refs work.
 * @author Andrew Jarombek
 * @since 1/15/2020
 */

import React, {createRef} from 'react';
import ButtonWrapper from './ButtonWrapper';
import ForwardRefButtonWrapper from './ForwardRefButtonWrapper';

const ForwardRefSample = () => {
  const buttonWrapperRef = createRef();
  const buttonWrapperForwardRef = createRef();

  return (
    <div>
      <ButtonWrapper
        onClick={() => console.info(buttonWrapperRef.current)}
        ref={buttonWrapperRef}>

        No Forward Ref
      </ButtonWrapper>
      <ForwardRefButtonWrapper
        onClick={() => console.info(buttonWrapperForwardRef.current)}
        ref={buttonWrapperForwardRef}>

        Forward Ref
      </ForwardRefButtonWrapper>
    </div>
  );
};

export default ForwardRefSample;
