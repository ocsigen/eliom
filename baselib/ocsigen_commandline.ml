(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception; 
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Ocsigen_config

let _ = Arg.parse
    [("-c", Arg.String set_configfile, 
      "Alternate config file (default /etc/ocsigen/ocsigen.conf)");
     ("--config", Arg.String set_configfile, 
      "Alternate config file (default /etc/ocsigen/ocsigen.conf)");
     ("-s", Arg.Unit set_silent, "Silent mode (error messages in errors.log only)");
     ("--silent", Arg.Unit set_silent, "Silent mode (error messages in errors.log only)");
     ("-p", Arg.String set_pidfile, "Specify a file where to write the PIDs of servers");
     ("--pidfile", Arg.String set_pidfile, "Specify a file where to write the PIDs of servers");
     ("-v", Arg.Unit set_verbose, "Verbose mode");
     ("--verbose", Arg.Unit set_verbose, "Verbose mode");
     ("-V", Arg.Unit set_veryverbose, "Very verbose mode (debug)");
     ("--veryverbose", Arg.Unit set_veryverbose, "Very verbose mode (debug)");
     ("-d", Arg.Unit set_daemon, "Daemon mode (detach the process)");
     ("--daemon", Arg.Unit set_daemon, "Daemon mode (detach the process) (This is the default when there are more than 1 process)");
     ("--version", Arg.Unit display_version, "Display version number and exit")
   ]
    (fun _ -> ())
    "usage: ocsigen [-c configfile]"

