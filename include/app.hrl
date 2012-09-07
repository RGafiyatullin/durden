%% 
%% This file is a part of Durden released under the MIT licence.
%% See LICENCE file for more infromation
%% 

-ifndef(app_hrl).
-define(app_hrl, included).

% -define(dict_t, orddict:orddict() ).
% -define(dict_m, orddict).

-define(dict_t, orddict:orddict() ).
-define(dict_m, orddict).

-define( log_common(Lvl, Report),
		error_logger:(
				case Lvl of
					debug -> info_report;
					info -> info_report;
					notice -> info_report;

					warning -> warning_report;

					error -> error_report;
					critical -> error_report;
					alert -> error_report;
					emergency -> error_report
				end
			)(Report)
	).

% info
-define( log_debug(Report), ?log_common(debug, Report) ).
-define( log_info(Report), ?log_common(info, Report) ).
-define( log_notice(Report), ?log_common(notice, Report) ).

% warning
-define( log_warn(Report), ?log_common(warning, Report) ).

% error
-define( log_error(Report), ?log_common(error, Report) ).
-define( log_crit(Report), ?log_common(critical, Report) ).
-define( log_alert(Report), ?log_common(alert, Report) ).
-define( log_fatal(Report), ?log_common(emergency, Report) ).


-endif. % app_hrl