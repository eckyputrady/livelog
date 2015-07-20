import {h} from '@cycle/web';
import moment from 'moment';
import _ from 'lodash';
import {Rx} from '@cycle/core';
import {loggedInContainer, circleLoader} from './common.js'

module.exports = {
  input, output
};

//// OUTPUT

function output (model) {
  return loggedInContainer(tagsView(model));
}

function tagsView (model) {
  return [
    h('div.row', h('div.col.s12', h('h3', 'Tags'))),
    h('div', [
      tagsListing(model),
      // h('div.center', circleLoader(model.tags.isLoading))
    ])
  ];
}

function tagsListing (model) {
  return h('ul.collection', [
    _.map(model.tags, tagItem)
  ]);
}

function tagItem (tag) {
  return h('li.collection-item', [
    h('div', [
      tag.name,
      h('a.secondary-content', h('i.material-icons', 'delete'))
    ])
  ]);
}

//// INPUT

function input (DOM) {
  return {
      // logout$: parseLogout(DOM),
      // createTag$: parseCreateTag(DOM),
      // changeState$: parseChangeState(DOM),
      // createTagging$: parseCreateTagging(DOM),
      // deleteTagging$: parseDeleteTagging(DOM)
  };
}