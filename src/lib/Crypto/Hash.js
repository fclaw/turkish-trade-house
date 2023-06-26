import sha256 from 'crypto-js/sha256';

export const _createHash = tm => { return () => { return sha256(tm); }; }