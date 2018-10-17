'use strict';

function Tabs() {
  var bindAll = function() {
    var menuElements = document.querySelectorAll('[data-tab]');
    var spoilerContent = document.querySelectorAll('.spoiler__content_show');
    for(var i = 0; i < menuElements.length ; i++) {
      menuElements[i].addEventListener('click', change, false);
    }
  }

  var clear = function() {
    var menuElements = document.querySelectorAll('[data-tab]');
    for(var i = 0; i < menuElements.length ; i++) {
      menuElements[i].classList.remove('active');
      var id = menuElements[i].getAttribute('data-tab');
      document.getElementById(id).classList.remove('active');
    }
  }

  var change = function(e) {
    clear();
    e.target.classList.add('active');
    var id = e.currentTarget.getAttribute('data-tab');
    document.getElementById(id).classList.add('active');

    // close all spoiler (spoiler.js)
    var spoilerBtn = document.querySelectorAll('.spoiler__button');
    var spoilerContent = document.querySelectorAll('.spoiler__content_show');
    for(var s = 0; s < spoilerContent.length ; s++) {
      spoilerContent[s].classList.remove('spoiler__content_show');
    }
    for(var b = 0; b < spoilerBtn.length ; b++) {
      spoilerBtn[b].classList.remove('active');
    }
  }

  bindAll();
}

var connectTabs = new Tabs();
