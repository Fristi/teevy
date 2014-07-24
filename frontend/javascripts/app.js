'use strict';

var TeevyApp = angular.module('teevyApp', ['ngCookies', 'ngRoute', 'truncate']);

var posterUrl = function (show, size) {
	return "https://image.tmdb.org/t/p/w" + size + show.showPosterPath;
};

var formatEpisodeId = function (episodeId) {
	return "S" + episodeId.seasonNr + " E" + episodeId.episodeNr;
};

TeevyApp.factory('teevyInterceptor', ['$q', '$cookies', function ($q, $cookies) {
	return {
		request: function (config) {
			config.headers = config.headers || {};
			if ($cookies && $cookies["teevy"]) {
				config.headers.Authorization = $cookies["teevy"];
			}
			return config;
		}
	};
}]);

TeevyApp.config(['$routeProvider', '$httpProvider', function ($routeProvider, $httpProvider) {

	$routeProvider
		.when('/dashboard', {
			templateUrl: "dashboard.html",
			controller: "dashboardCtrl"
		})
		.when('/explore', {
			templateUrl: "explore.html",
			controller: "exploreCtrl"
		})
		.otherwise({
			redirectTo: '/dashboard'
		});

	$httpProvider.interceptors.push('teevyInterceptor');
}]);

// controllers

TeevyApp.controller('dashboardCtrl', ['$scope', 'Subscription', function ($scope, Subscription) {
	$scope.model = { subscriptions: [] };

	$scope.posterUrl = posterUrl;

	$scope.renderSubscriptions = function (resp) {
		$scope.model.subscriptions = resp.data.data;
	};

	Subscription.all().then($scope.renderSubscriptions);
}]);

TeevyApp.controller('exploreCtrl', ['$scope', 'Show', 'Subscription', function ($scope, Show, Subscription) {
	Show.all()
		.then(function (resp) {
			//TODO error check..
			$scope.explore = resp.data.data;
		});

	$scope.posterUrl = posterUrl;

	$scope.subscribe = function (ss) {
		ss.pending = true;
		ss.isSubscribed = !ss.isSubscribed;
		Subscription.create(ss.subscribedTo.showId)
			.then(function (response) {
				ss.pending = false;
			});
	}

	$scope.unsubscribe = function (ss) {
		ss.pending = true;
		ss.isSubscribed = !ss.isSubscribed;
		Subscription.delete(ss.subscribedTo.showId)
			.then(function () {
				ss.pending = false;
			});
	}
}]);

TeevyApp.controller('userCtrl', ['$scope', 'User', function ($scope, User) {
	User.getUserInfo()
		.then(function (response) {
			$scope.user = response.data.data;
		});

	$scope.logout = User.logoutUser;
	$scope.showMenu = false;
}]);

TeevyApp.controller('appCtrl', ['$scope', '$location', '$route', 'Show', 'Subscription', function ($scope, $location, $route, Show, Subscription) {
	Show.all()
		.then(function (data) {
			$scope.shows = data.data;
		});

	$scope.posterUrl = posterUrl;
	$scope.showSearchResults = false;

	$scope.isActive = function (value) {
		if ($location.path().slice(1) === value) return 'active';
	};

	$scope.subscribe = function (ss) {
		$scope.search = "";
		ss.pending = true;
		ss.isSubscribed = !ss.isSubscribed;
		Subscription.create(ss.showId)
			.then(function () {
				$route.reload();
			});
	};

	$scope.unsubscribe = function (ss) {
		$scope.search = "";
		ss.pending = true;
		ss.isSubscribed = !ss.isSubscribed;
		Subscription.delete(ss.showId)
			.then(function () {
				$route.reload();
			});
	};

	$scope.$watch('search', function (s) {
		if (s && s.length >= 3) {
			Show.search(s).then(function (resp) {
				$scope.foundShows = resp.data.data.foundShows;
				$scope.showSearchResults = true;
			});
		} else {
			$scope.showSearchResults = false;
		}
	});
}]);

// services

TeevyApp.factory('User', ['$http', '$log', '$cookies', function ($http, $log, $cookies) {
	return {
		getUserInfo: function () {
			return $http.get('/api/user/info');
		},
		logoutUser: function () {
			delete $cookies["teevy"];
            window.location = '/';
		}
	}
}]);

TeevyApp.factory('Show', ['$http', '$q', '$log', function ($http, $q, $log) {
	return {
		all: function () {
			return $http.get('/api/explore');
		},
		search: function (term) {
			return $http.post('/api/search', { searchTerm: term });
		},
		get: function (id) {
			// alert("Kannie");
		}
	}
}]);

TeevyApp.factory("Episode", ['$http', '$q', '$log', function ($http, $q, $log) {
	return {
		all: function (show_id) {
			return $http.get('/api/show/' + show_id + '/episodes');
		}
	}
}]);

TeevyApp.factory('Subscription', ['$http', '$q', '$log', 'Episode', function ($http, $q, $log, Episode) {
	return {
		all: function () {
			return $http.get('/api/subscriptions');
		},
		get: function (id) {
			if (cache[id]) {
				var deferred = $q.defer();
				deferred.resolve(cache[id]);
				return deferred.promise;
			} else {
				return api.all()
					.then(function () {
						return cache[id];
					});
			}
		},
		next: function (id) {
			return $http.post('/api/subscription/progression/next', {currentEpisodeId: id});
		},
		update: function (id) {
			return $http.post('/api/subscription/progression', {progression: id});
		},
		create: function (show_id) {
			return $http.post('/api/subscription/add', {
				subscribeShowId: show_id
			});
		},
		delete: function (show_id) {
			return $http.post('/api/subscription/remove', {
				unsubscribeShowId: show_id
			});
		}
	}
}]);

// directives

TeevyApp.directive('dropdown', ['$document', function ($document) {
	return {
		scope: {
			open: '='
		},
		link: function (scope, element, attributes) {
			var dropdownEl = element.find('.dropdown');

			scope.$watch('open', function (value) {
				if (value) {
					dropdownEl.show();
					$document.bind('click', outsideClickHandler);
				} else {
					dropdownEl.hide();
					$document.unbind('click', outsideClickHandler);
				}
			});

			function outsideClickHandler(event) {
				if (!element.is(event.target) && !element.find(event.target).length) {
					scope.open = false;
					scope.$apply();
				}
			}
		}
	}
}]);

TeevyApp.directive('delayedModel', function () {
	return {
		scope: {
			model: '=delayedModel'
		},
		link: function (scope, element, attrs) {

			element.val(scope.model);

			scope.$watch('model', function (newVal, oldVal) {
				if (newVal !== oldVal) {
					element.val(scope.model);
				}
			});

			var timeout;
			element.on('keyup paste search', function () {
				clearTimeout(timeout);
				timeout = setTimeout(function () {
					scope.model = element[0].value;
					element.val(scope.model);
					scope.$apply();
				}, attrs.delay || 500);
			});
		}
	};
});

TeevyApp.directive('subscriptionTile', ['$log', '$timeout', 'Show', 'Episode', 'Subscription', function ($log, $timeout, Show, Episode, Subscription) {
	return {
		restrict: 'E',
		templateUrl: 'subscription-tile.html',
		scope: {
			subscription: '=',
			subscriptions: '=',
			state: '@'
		},
		link: function (scope, element, attrs) {

			scope.initialized = true;
			scope.config = scope.state == 'unconfigured';
			scope.posterUrl = posterUrl;
			scope.formatEpisodeId = formatEpisodeId;

			scope.isAired = function (ep) {
				if (!ep.episodeReleaseDate) {
					return false;
				}
				var releaseDate = moment(ep.episodeReleaseDate);
				var now = moment();
				var diff = releaseDate.diff(now, 'days');
				return diff <= 0;
			};

			scope.isToday = function (x, y) {
				return x.get('year') == y.get('year') && x.get('month') == y.get('month') && x.get('date') == y.get('date');
			};

			scope.timeLeft = function (ep) {
				var now = moment().utc();
				var releaseDate = moment(ep.episodeReleaseDate).utc();
				var diffInDays = releaseDate.diff(now, 'days');

				if (diffInDays > 28) {
					return releaseDate.format("Do MMM YYYY");
				}
				if (diffInDays == 0) {
					return scope.isToday(now, releaseDate) ? 'Today' : 'Tomorrow';
				}
				return releaseDate.from(now);
			};

			scope.getShow = function () {
				switch (scope.state) {
					case 'finished':
					case 'unfinished':
						return scope.subscription;
						break;

					case 'to-air':
					case 'to-watch':
					case 'unconfigured':
						return scope.subscription.show;

						break;
				}
				return null;
			};

			scope.getLastEpisode = function () {
				switch (scope.state) {
					case 'unconfigured':
						return scope.subscription.lastEpisode;
						break;
				}
				return null;
			};

			scope.getNextEpisode = function () {
				switch (scope.state) {

					case 'to-air':
					case 'to-watch':
						return scope.subscription.nextEpisode;
						break;
				}
				return null;
			};

			scope.isToAir = function () {
				return scope.state == 'to-air';
			};

			scope.isUnconfigured = function () {
				return scope.state == 'unconfigured';
			};

			scope.isToWatch = function () {
				return scope.state == 'to-watch';
			};

			scope.isFinished = function () {
				return scope.state == 'finished';
			};

			scope.isUnfinished = function () {
				return scope.state == 'unfinished';
			};

			scope.selectEpisode = function (episode) {
				if (scope.isAired(episode)) {
					scope.selected_episode = episode;
				}
			};

			scope.setFirstEpisode = function () {
				var show = scope.getShow();
				if (show == null) return;
				scope.exitConfig();
				var cmd = {
					showId: show.showId,
					episodeNr: 1,
					seasonNr: 1
				};
				Subscription.update(cmd)
					.then(function (response) {
						scope.subscriptions.subscriptions = response.data.data;
					});
			}

			scope.nextEpisode = function () {
				var ep = scope.getNextEpisode();

				if (ep == null) {
					return;
				}
				Subscription.next(ep.episodeId)
					.then(function (response) {
						scope.subscriptions.subscriptions = response.data.data;
						scope.pending = false;
					})
			};

			scope.setEpisode = function (eid) {
				scope.exitConfig();
				Subscription.update(eid)
					.then(function (response) {
						scope.subscriptions.subscriptions = response.data.data;

					});
			};

			scope.enterConfig = function () {
				scope.manual = false;
				scope.config = true;
			};

			scope.exitConfig = function () {
				scope.manual = false;
				scope.config = false;
			}

			scope.enterManual = function () {
				var show = scope.getShow();
				if (show == null) return;
				Episode.all(show.showId)
					.then(function (response) {
						scope.episodes = response.data.data.episodes;
						scope.manual = true;
					});
			}

			scope.unsubscribe = function () {
				scope.pending = true;
				var show = scope.getShow();
				if (show == null) return;
				Subscription.delete(show.showId)
					.then(function (response) {
						scope.subscriptions.subscriptions = response.data.data;
					})
			}
		}
	}
}]);

// filters

TeevyApp.filter('date', function () {
	return function (input, f) {
		if (moment(input).isValid()) {
			return moment(input).format(f)
		}
		return input;
	}
});

// utilities

function isNumeric(n) {
	return !isNaN(parseFloat(n)) && isFinite(n);
}

function getValuesArr(obj) {
	var res = [];

	for (key in obj) {
		if (obj.hasOwnProperty(key)) {
			res.push(obj[key]);
		}
	}

	return res;
}

