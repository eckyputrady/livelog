import {h} from '@cycle/web';
import {fab,modals} from './modals.js';
import navbar from './navbar.js';

module.exports = {
  loggedInContainer,
  circleLoader
};

////

function loggedInContainer (content) {
  return h('div', [
    navbar.output(true),
    content,
    fab(),
    modals()
  ]);
}

function circleLoader (isActive, size) {
  let active = isActive ? '.active' : '';
  let sizeClass = '.' + size;
  return h('div.preloader-wrapper' + active + sizeClass, h('div.spinner-layer.spinner-blue-only', [
    h('div.circle-clipper.left', h('div.circle')),
    h('div.gap-patch', h('div.circle')),
    h('div.circle-clipper.right', h('div.circle'))
  ]));
}