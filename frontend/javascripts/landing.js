'use strict';

var TeevyLanding = angular.module('teevyLanding', ['ngRoute', 'ngCookies']);

TeevyLanding.controller('appCtrl', ['$log', '$http', '$cookies', function ($log, $http, $cookies) {

	var ac = this;

	if ($cookies != null && $cookies["teevy"]) {
		window.location = "/app.html"
	}

	ac.modalType = null;
	ac.errors = {};

	ac.facebookAuthenticated = function(resp) {
		$http.post('/api/user/auth/facebook', resp.authResponse).then(ac.handleSuccessResponse);
	};

	ac.resetErrors = function () {
		ac.errors = [];
	};

	ac.changeModalType = function (value) {
		ac.resetErrors();
		ac.password = "";
		ac.modalType = value;
	};

	ac.processResponse = function (data) {
		if (data == null || data.status == null) {
			ac.unableToRetrieveData();
			return true;
		} else if (data.status == 'error') {
			ac.errors = [data.message];
			return true;
		} else if (data.status == 'fail') {
			ac.errors = [];
			for (var i = 0; i < data.data.length; i++) {
				ac.errors.push(data.data[i]["formError"]);
			}
			return true;
		}
		return false;
	};

	ac.unableToRetrieveData = function () {
		ac.errors = ["Unable to retrieve data"];
	};

	ac.handleSuccessResponse = function (response) {
		if (!ac.processResponse(response.data)) {
			$cookies["teevy"] = response.data.data.token;
			window.location = '/app.html';
		}
	};

	ac.facebookLogin = function () {
		FB.login(function (response) {
			console.log(response);
			if (response.authResponse) {
				FB.api('/me', function (response) {
					console.log(response);
					console.log('Good to see you, ' + response.name + '.');
				});
			}
		}, { scope: "email" });
	};

	ac.logIn = function () {
		$http.post('/api/user/auth/teevy', {
			login: ac.login,
			loginPassword: ac.password
		}).then(ac.handleSuccessResponse, ac.unableToRetrieveData);
	};

	ac.signUp = function () {
		$http.post('/api/user/register', {
			registerNickname: ac.nickname,
			registerEmail: ac.email,
			registerPassword: ac.password
		}).then(ac.handleSuccessResponse, ac.unableToRetrieveData);
	};
}]);

TeevyLanding.directive('modal', ['$document', function ($document) {
	return {
		scope: {
			modal: '='
		},
		link: function (scope, element, attrs) {
			var modalOpen = false;

			$document.bind('click', outsideClickHandler);

			function outsideClickHandler(event) {
				if (modalOpen) {
					if (!element.is(event.target) && !element.find(event.target).length) {
						scope.modal = null;
						scope.$apply();
						$document.unbind('click', outsideClickHandler);
					}
				} else {
					modalOpen = true;
				}
			}
		}
	}
}]);

TeevyLanding.directive('passwordConfirmation', function () {
	return function (scope, element, attrs) {
		var input_password = element.find('.password')[0],
			input_confirmation = element.find('.confirmation')[0];

		input_password.onchange = validatePassword;
		input_confirmation.onchange = validatePassword;

		function validatePassword() {
			if (input_password.value != input_confirmation.value) {
				input_confirmation.setCustomValidity("Passwords Don't Match");
			} else {
				input_confirmation.setCustomValidity('');
			}
		}
	}
});