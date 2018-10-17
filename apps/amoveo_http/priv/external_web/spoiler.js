'use strict';

(function () {

	var toggleSpoilerVisibility = function (element) {
	  element.addEventListener('click', function () {
	    this.classList.toggle('active');
	    this.nextElementSibling.classList.toggle('spoiler__content_show');
	  });
	}

	var allSpoilerButton = Array.prototype.slice.call(document.querySelectorAll('.spoiler__button'));

	allSpoilerButton.forEach(toggleSpoilerVisibility);
}());
