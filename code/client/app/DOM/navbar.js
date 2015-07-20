import {h} from '@cycle/web';

module.exports = {
  input, output
}

//// OUTPUT

function output (withSideNav) {
  return h('nav', [
    h('div.container', [
      sideNav(withSideNav),
      h('div.nav-wrapper', [
        h('a.brand-logo', 'LiveLog')
      ])
    ])
  ]);
}

function sideNav (visible) {
  let extraClass = visible ? '' : '.hide';
  return [
    h('ul.right.hide-on-med-and-down' + extraClass, sideNavContent()),
    h('ul#sideNav.side-nav', sideNavContent()),
    h('a.button-collapse' + extraClass, 
      {'hook-init': new initSidenavHook(), attributes:{'data-activates':'sideNav'}}, 
      h('i.mdi-navigation-menu'))
  ];
}

function sideNavContent () {
  return [
    h('li', h('a#change-state', {attributes:{'data-nextState':'Logs'}}, 'Logs')),
    h('li', h('a#change-state', {attributes:{'data-nextState':'Tags'}}, 'Tags')),
    h('li.divider'),
    h('li', h('a#logout', 'Logout'))
  ];
}

function initSidenavHook () {}
initSidenavHook.prototype.hook = (node, name, prevVal) => {
  if (!prevVal) { 
    setTimeout(() => $(node).sideNav(), 1000);
  }
}

function hideSidenav () {
  $('a.button-collapse').sideNav('hide');
}

//// INPUT

function input(DOM) {
  return {
    logout$: parseLogout(DOM).do(hideSidenav),
    setState$: parseChangeState(DOM).do(hideSidenav)
  };
}

function parseLogout (DOM) {
  return DOM.get('a#logout', 'click');
}

function parseChangeState (DOM) {
  return DOM.get('a#change-state', 'click')
    .map(e => $(e.target).data('nextstate'));
}