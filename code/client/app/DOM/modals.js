import {h} from '@cycle/web';
import moment from 'moment';
import _ from 'lodash';
import {Rx} from '@cycle/core';
import {navbar} from './common.js'

module.exports = {
  input, fab, modals
};

//// OUTPUT

function fab () {
  return h('div.fixed-action-btn', {style:{bottom:'45px',right:'24px'}}, [
    h('a.btn-floating.btn-large.red', h('i.large.material-icons', 'add')),
    h('ul', [
      h('li', h('a.modal-trigger.btn-floating.red', {'modal-hook': new initModalHook(), href:'#log-dialog'}, h('i.small.material-icons', 'done'))),
      h('li', h('a.modal-trigger.btn-floating.red', {'modal-hook': new initModalHook(), href:'#tag-dialog'}, h('i.small.material-icons', 'label')))
    ])
  ]);
}

function modals () {
  return [
    logDialogModal(),
    tagDialogModal()
  ];
}

function modal (id, content, footer) {
  return h('div#' + id + '.modal', [
    h('div.modal-content', content),
    h('div.modal-footer', footer)
  ]);
}

function logDialogModal () {
  return modal('log-dialog', [
    h('h4', 'Log An Activity'),
    h('div.row', [
      h('div.input-field.s12', [
        h('input#create-log-name', {type:'text'}),
        h('label', 'Activity name')
      ])
    ])
  ], [
    h('a#create-log.modal-action.modal-close.waves-effect.waves-green.btn-flat', 'Create'),
    h('a.modal-action.modal-close.waves-effect.waves-green.btn-flat', 'Cancel'),
  ]);
}

function tagDialogModal () {
  return modal('tag-dialog', [
    h('h4', 'Create Tag'),
    h('form', [
      h('div.input-field', [
        h('input#create-tag-name', {type:'text'}),
        h('label', 'Tag name')
      ])
    ])
  ], [
    h('a#create-tag.modal-action.modal-close.waves-effect.waves-green.btn-flat', 'Create'),
    h('a.modal-action.modal-close.waves-effect.waves-green.btn-flat', 'Cancel'),
  ]);
}

function initModalHook () {}
initModalHook.prototype.hook = (node, propname, prevVal) => {
  if (!prevVal) {
    $(node).leanModal();
  }
}

//// INPUT

function input (DOM) {
  return {
    createLog$: parseCreateLog(DOM),
    createTag$: parseCreateTag(DOM)
  };
}

function parseCreateLog (DOM) {
  let id = '#create-log-name';
  let btnId = '#create-log';
  let modalId = '#log-dialog';
  let clicks = DOM.get(btnId + ':not(.disabled)', 'click');
  let enters = DOM.get(id, 'keyup').filter(e => e.keyCode === 13).do(() => $(modalId).closeModal());
  let merged = Rx.Observable.merge(clicks, enters);

  return merged.map(() => {
    return {
      message: $(id).val()
    };
  });
}

function parseCreateTag (DOM) {
  let id = '#create-tag-name';
  let btnId = '#create-tag';
  let modalId = '#tag-dialog';
  let clicks = DOM.get(btnId + ':not(.disabled)', 'click');
  let enters = DOM.get(id, 'keyup').filter(e => e.keyCode === 13).map(() => $(modalId).closeModal());
  let merged = Rx.Observable.merge(clicks, enters);

  return merged.map(() => {
    return {
      name: $(id).val()
    };
  });
}