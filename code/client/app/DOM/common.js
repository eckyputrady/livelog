import {h} from '@cycle/web';
import {fab,modals} from './modals.js';
import navbar from './navbar.js';

module.exports = {
  loggedInContainer
}

function loggedInContainer (content) {
  return h('div', [
    navbar.output(true),
    content,
    fab(),
    modals()
  ]);
}