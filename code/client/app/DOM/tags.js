import {h} from '@cycle/web';
import _ from 'lodash';
import {loggedInContainer} from './common.js';

module.exports = {
  input, output
};

//// OUTPUT

function output (model) {
  return loggedInContainer(tagsView(model));
}

function tagsView (model) {
  return [
    h('div.container', h('h3', 'Tags')),
    h('div.container', [
      tagsListing(model),
      // h('div.center', circleLoader(model.tags.isLoading))
    ])
  ];
}

function tagsListing (model) {
  return h('ul.collection.z-depth-1', [
    _.map(model.tags, tagItem)
  ]);
}

function tagItem (tag) {
  return h('li.collection-item', [
    h('div', [
      tag.name,
      // h('a.secondary-content', h('i.material-icons', 'delete'))
    ])
  ]);
}

//// INPUT

function input () {
  return {};
}