(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2007 Vincent Balat
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

module Discard : Eliom_state_sigs.DISCARD

module Group   : Eliom_state_sigs.GROUP

module Expire  : Eliom_state_sigs.EXPIRE

module Timeout : Eliom_state_sigs.TIMEOUT

module Ext     : Eliom_state_sigs.EXT
  with type ('a, 'b) state = ('a, 'b) Eliom_state_base.Ext.state
