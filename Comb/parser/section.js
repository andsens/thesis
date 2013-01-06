// Generated by CoffeeScript 1.4.0
(function() {
  var __hasProp = {}.hasOwnProperty,
    __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; };

  define(['comb/mustache', 'comb/partial', 'comb/escaped_variable', 'comb/unescaped_variable', 'exports'], function(Mustache, Partial, EscapedVariable, UnescapedVariable, exports) {
    'use strict';

    var Section;
    Section = (function(_super) {

      __extends(Section, _super);

      function Section() {
        return Section.__super__.constructor.apply(this, arguments);
      }

      Section.prototype.initialize = function() {
        var child, id;
        Section.__super__.initialize.apply(this, arguments);
        this.children = (function() {
          var _i, _len, _ref, _results;
          _ref = this.spec;
          _results = [];
          for (id = _i = 0, _len = _ref.length; _i < _len; id = ++_i) {
            child = _ref[id];
            if (child.section === this.id) {
              _results.push({
                id: id,
                child: child
              });
            }
          }
          return _results;
        }).call(this);
        return this.iterations = [];
      };

      Section.prototype.parse = function() {
        var childNodeOffset, content, i, id, iteration, j, last_first_joined, last_first_offset, last_next_joined, last_next_offset, nextMatched, obj, offset, offsetNode, offsetNodeId, prev_first_joined, prev_first_offset, prev_next_joined, prev_next_offset, type, _i, _j, _k, _len, _len1, _len2, _ref, _ref1, _ref10, _ref11, _ref12, _ref13, _ref14, _ref15, _ref16, _ref17, _ref2, _ref3, _ref4, _ref5, _ref6, _ref7, _ref8, _ref9;
        Section.__super__.parse.apply(this, arguments);
        prev_first_joined = false;
        prev_next_joined = false;
        last_first_joined = false;
        last_next_joined = false;
        if (this.id !== 0) {
          prev_first_joined = ((_ref = this.prev.type) === 'text' || _ref === 'escaped') && ((_ref1 = this.first.type) === 'text' || _ref1 === 'escaped');
          prev_next_joined = ((_ref2 = this.prev.type) === 'text' || _ref2 === 'escaped') && ((_ref3 = this.next.type) === 'text' || _ref3 === 'escaped');
          last_first_joined = ((_ref4 = this.last.type) === 'text' || _ref4 === 'escaped') && ((_ref5 = this.first.type) === 'text' || _ref5 === 'escaped');
          last_next_joined = ((_ref6 = this.last.type) === 'text' || _ref6 === 'escaped') && ((_ref7 = this.next.type) === 'text' || _ref7 === 'escaped');
          prev_first_joined = prev_first_joined || this.prev.type === 'null';
          prev_next_joined = prev_next_joined || this.prev.type === 'null';
          prev_first_offset = prev_first_joined ? 0 : 1;
          prev_next_offset = prev_next_joined ? 0 : 1;
          last_first_offset = last_first_joined ? 0 : 1;
          last_next_offset = last_next_joined ? 0 : 1;
          if (!this.nodeMatches('first', this.first, prev_first_offset)) {
            if (!this.nodeMatches('next', this.next, prev_next_offset)) {
              throw new Error("Unable to match the next node");
            }
            return;
          }
        }
        i = 0;
        while (true) {
          iteration = {};
          _ref8 = this.children;
          for (_i = 0, _len = _ref8.length; _i < _len; _i++) {
            _ref9 = _ref8[_i], id = _ref9.id, (_ref10 = _ref9.child, type = _ref10.type, offset = _ref10.offset);
            if (!(!offset)) {
              continue;
            }
            childNodeOffset = this.nodeOffset;
            if (this.id !== 0) {
              childNodeOffset += i === 0 ? prev_first_offset : last_first_offset;
            }
            switch (type) {
              case 'section':
                obj = new Section(id, this.spec, this.partials, this.parent, childNodeOffset, this.strOffset);
                break;
              case 'partial':
                obj = new Partial(id, this.spec, this.partials, this.parent, childNodeOffset, this.strOffset);
                break;
              case 'escaped':
                obj = new EscapedVariable(id, this.spec, this.partials, this.parent, childNodeOffset, this.strOffset);
                break;
              case 'unescaped':
                obj = new UnescapedVariable(id, this.spec, this.partials, this.parent, childNodeOffset, this.strOffset);
            }
            iteration[id] = obj;
          }
          _ref11 = this.children;
          for (_j = 0, _len1 = _ref11.length; _j < _len1; _j++) {
            _ref12 = _ref11[_j], id = _ref12.id, (_ref13 = _ref12.child, type = _ref13.type, offset = _ref13.offset, (_ref14 = _ref13.path, offsetNodeId = _ref14[0]));
            if (!(offset)) {
              continue;
            }
            offsetNode = iteration[offsetNodeId];
            if (offsetNode == null) {
              throw new Error("Something is wrong with the ordering of the offset children");
            }
            switch (type) {
              case 'section':
                obj = new Section(id, this.spec, this.partials, offsetNode.parent, offsetNode.nodeOffset, offsetNode.strOffset);
                break;
              case 'partial':
                obj = new Partial(id, this.spec, this.partials, offsetNode.parent, offsetNode.nodeOffset, offsetNode.strOffset);
                break;
              case 'escaped':
                obj = new EscapedVariable(id, this.spec, this.partials, offsetNode.parent, offsetNode.nodeOffset, offsetNode.strOffset);
                break;
              case 'unescaped':
                obj = new UnescapedVariable(id, this.spec, this.partials, offsetNode.parent, offsetNode.nodeOffset, offsetNode.strOffset);
            }
            iteration[id] = obj;
          }
          this.iterations.push(iteration);
          if (this.id === 0) {
            break;
          }
          _ref15 = this.contents;
          for (j = _k = 0, _len2 = _ref15.length; _k < _len2; j = ++_k) {
            content = _ref15[j];
            switch (content) {
              case '#node':
              case '#emptynode':
              case "#comment":
                this.strOffset = 0;
                if (i === 0 && j === 0 && prev_first_joined) {
                  break;
                }
                this.nodeOffset += 1;
                break;
              case '#text':
                if (j === 0) {
                  if (i === 0 && prev_first_joined) {
                    break;
                  }
                  if (i > 0 && last_first_joined) {
                    break;
                  }
                }
                if (!(this.contents[j - 1] != null) || ((_ref16 = this.contents[j - 1]) === '#node' || _ref16 === '#emptynode' || _ref16 === '#comment')) {
                  this.nodeOffset += 1;
                }
                break;
              default:
                offsetNode = iteration[content];
                this.nodeOffset = offsetNode.nodeOffset;
                this.strOffset = offsetNode.strOffset;
            }
          }
          if (!this.nodeMatches('last', this.last)) {
            throw new Error("Unable to verify last element in iteration");
          }
          if ((_ref17 = this.last.type) === 'text' || _ref17 === 'escaped') {
            this.strOffset += this.last.value.length;
          }
          nextMatched = this.nodeMatches('next', this.next, last_next_offset);
          i++;
          if (this.nodeMatches('first', this.first, last_first_offset)) {
            if (nextMatched) {
              throw new Error("Was able to both match a continuation and an end of the iteration");
            }
          } else {
            if (!nextMatched) {
              throw new Error("Unable to match the next node");
            }
            break;
          }
        }
        if (i === 0) {
          return this.nodeOffset += prev_next_offset;
        } else {
          return this.nodeOffset += last_next_offset;
        }
      };

      Section.prototype.getRoot = function() {
        var id, item, iteration, name, object, values, _i, _len, _ref, _ref1;
        object = [];
        _ref = this.iterations;
        for (_i = 0, _len = _ref.length; _i < _len; _i++) {
          iteration = _ref[_i];
          values = {};
          for (id in iteration) {
            item = iteration[id];
            name = item.name;
            if (item.inverted) {
              name = "^" + item.name;
            }
            if ((_ref1 = values[name]) == null) {
              values[name] = [];
            }
            values[name].push(item.getRoot());
          }
          object.push(values);
        }
        return {
          type: 'section',
          iterations: object
        };
      };

      Section.prototype.getValues = function(merge) {
        var i, id, item, iteration, name, val, values, _i, _len, _ref, _ref1, _ref2;
        if (merge == null) {
          merge = [];
        }
        _ref = this.iterations;
        for (i = _i = 0, _len = _ref.length; _i < _len; i = ++_i) {
          iteration = _ref[i];
          if ((_ref1 = merge[i]) == null) {
            merge[i] = {};
          }
          values = merge[i];
          for (id in iteration) {
            item = iteration[id];
            name = item.name;
            if (item.inverted) {
              name = "^" + item.name;
            }
            switch (item.type) {
              case 'section':
                values[name] = item.getValues(values[name]);
                break;
              case 'partial':
                values = item.getValues(values);
                break;
              default:
                values[name] = item.getValues(values[name]);
            }
          }
        }
        if (this.iterations.length === 1) {
          _ref2 = merge[0];
          for (id in _ref2) {
            val = _ref2[id];
            merge[id] = val;
          }
        }
        return merge;
      };

      return Section;

    })(Mustache);
    return exports.Section = Section;
  });

}).call(this);
