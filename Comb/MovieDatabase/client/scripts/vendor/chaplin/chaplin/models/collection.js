// Generated by CoffeeScript 1.4.0
(function() {
  var __hasProp = {}.hasOwnProperty,
    __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; };

  define(['underscore', 'backbone', 'chaplin/lib/event_broker', 'chaplin/models/model'], function(_, Backbone, EventBroker, Model) {
    'use strict';

    var Collection;
    return Collection = (function(_super) {

      __extends(Collection, _super);

      function Collection() {
        return Collection.__super__.constructor.apply(this, arguments);
      }

      _(Collection.prototype).extend(EventBroker);

      Collection.prototype.model = Model;

      Collection.prototype.initDeferred = function() {
        return _(this).extend($.Deferred());
      };

      Collection.prototype.serialize = function() {
        var model, _i, _len, _ref, _results;
        _ref = this.models;
        _results = [];
        for (_i = 0, _len = _ref.length; _i < _len; _i++) {
          model = _ref[_i];
          if (model instanceof Model) {
            _results.push(model.serialize());
          } else {
            _results.push(model.toJSON());
          }
        }
        return _results;
      };

      Collection.prototype.addAtomic = function(models, options) {
        var direction, model;
        if (options == null) {
          options = {};
        }
        if (!models.length) {
          return;
        }
        options.silent = true;
        direction = typeof options.at === 'number' ? 'pop' : 'shift';
        while (model = models[direction]()) {
          this.add(model, options);
        }
        return this.trigger('reset');
      };

      Collection.prototype.update = function(models, options) {
        var fingerPrint, i, ids, model, newFingerPrint, preexistent, _i, _ids, _len;
        if (options == null) {
          options = {};
        }
        fingerPrint = this.pluck('id').join();
        ids = _(models).pluck('id');
        newFingerPrint = ids.join();
        if (newFingerPrint !== fingerPrint) {
          _ids = _(ids);
          i = this.models.length;
          while (i--) {
            model = this.models[i];
            if (!_ids.include(model.id)) {
              this.remove(model);
            }
          }
        }
        if (newFingerPrint !== fingerPrint || options.deep) {
          for (i = _i = 0, _len = models.length; _i < _len; i = ++_i) {
            model = models[i];
            preexistent = this.get(model.id);
            if (preexistent) {
              if (options.deep) {
                preexistent.set(model);
              }
            } else {
              this.add(model, {
                at: i
              });
            }
          }
        }
      };

      Collection.prototype.disposed = false;

      Collection.prototype.dispose = function() {
        var prop, properties, _i, _len;
        if (this.disposed) {
          return;
        }
        this.trigger('dispose', this);
        this.reset([], {
          silent: true
        });
        this.unsubscribeAllEvents();
        this.off();
        if (typeof this.reject === "function") {
          this.reject();
        }
        properties = ['model', 'models', '_byId', '_byCid', '_callbacks'];
        for (_i = 0, _len = properties.length; _i < _len; _i++) {
          prop = properties[_i];
          delete this[prop];
        }
        this.disposed = true;
        return typeof Object.freeze === "function" ? Object.freeze(this) : void 0;
      };

      return Collection;

    })(Backbone.Collection);
  });

}).call(this);
