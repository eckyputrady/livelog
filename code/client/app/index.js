"use strict";

require('jquery');
require('npm/materialize-css/bin/materialize.css');
require('npm/materialize-css/bin/materialize.js');
import Cycle from '@cycle/core';
import {h, makeDOMDriver} from '@cycle/web';

document.querySelector('body').appendChild(document.createElement('div'));

function view () {
  return h('div', [
    navbar(),
    content(),
    fab(),
    createTagModal()
  ]);
}

function fab () {
  return h('div.fixed-action-btn', {style:{bottom:'45px',right:'24px'}}, [
    h('a.btn-floating.btn-large.waves-effect.waves-light.red',[
      h('span.material-icons', '+')
    ])
  ]);
}

function content () {
  return h('div.container', [
    h('div.section', currentLog()),
    h('div.section', pastActivities())
  ])
}

function pastActivities () {
  return h('div.row', [
    h('div.col.s12', [
      h('table', [
        h('thead', [
          h('tr', [
            h('th', 'Message'),
            h('th', 'Tags'),
            h('th', 'Duration'),
          ])
        ]),
        h('tbody', [
          h('tr', [
            h('td', 'Writing front end code'),
            h('td', labels()),
            h('td', '01:00:20')
          ])
        ])
      ])
    ])
  ])
}

function currentLog () {
  return h('div.row', [
    h('div.col.s8', h('h2', 'I do this!')),
    h('div.col.s4.valign-wrapper', h('h2.valign', '12:33:10')),
    h('div.col.s12'),
    labels()
  ]);
}

function labels () {
  return [
    label('haskell'),
    label('rest'),
    labelInput()
  ]
}

function labelInput () {
  setTimeout(initDropdown, 200);
  var randId = 'dropdown-' + new Date().getTime();
  return h('span', [
    h('a.dropdown-button.teal.lighten-4', {href:'#', attributes:{'data-activates':randId}, style:{padding:'3px', margin:'2px'}}, 'add labels'),
    h('ul#' + randId + '.dropdown-content', [
      h('li', h('a.modal-trigger', {href:'#create-tag-modal'}, 'create new tag')),
      h('li.divider'),
      h('li', h('a', 'one')),
      h('li', h('a', 'one')),
      h('li', h('a', 'one'))
    ])
  ]);
}

function createTagModal () {
  setTimeout(initModal, 200);
  return h('div#create-tag-modal.modal', [
    h('div.modal-content', [
      h('h4', 'Create a New Tag')
    ]),
    h('div.modal-footer', [
      h('a.modal-action.waves-effect.btn-flat', 'Create')
    ])
  ]);
}

function initModal () {
  console.log('called!');
  $('.modal-trigger').leanModal();
}

function initDropdown () {
  $('.dropdown-button').dropdown({
      inDuration: 300,
      outDuration: 225,
      constrain_width: false,
      gutter: 0, // Spacing from edge
      belowOrigin: false // Displays dropdown below the button
    }
  );
}

function label (name) {
  return h('span.teal.lighten-3', {style:{padding:'3px', margin:'2px'}}, name)
}

function navbar () {
  return h('nav', [
    h('div.container.nav-wrapper', {type:'checkbox'}, [
      h('a.brand-logo', 'LOGO')
    ])
  ]);
}

function main (responses) {
  return {
    DOM: responses.DOM.get('input', 'change')
          .map(ev => ev.target.checked)
          .startWith(false)
          .map(view)
  };
}

Cycle.run(main, {
  DOM: makeDOMDriver('body > div')
});