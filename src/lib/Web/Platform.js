import Bowser, * as e from './Bowser/src/bowser.js';

export const getPlatform = () => { return function () { Bowser.getParser().getPlatformType()}; };