function cmsvlib, version=version, query=query
;+
; NAME:
;   CMSVLIB
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Initialize the CMSVLIB save library
;
; CALLING SEQUENCE:
;   VALUE = CMSVLIB(/QUERY, VERSION=version)
;   
; DESCRIPTION: 
;
;   This function initializes the CMSVLIB library to read, write and
;   interrogate IDL save files.  Use the QUERY keyword to determine
;   whether the full CMSVLIB library is present.
;
;   The VERSION keyword allows the user to query the version number of
;   the CMSVLIB library.  The library version number will be returned
;   as a string of the form "X.Y" where X is the major version number
;   and Y is the minor version number.  Callers can use this version
;   number to decide whether this particular version of the library is
;   compatible with their usage.
;
;
;   The procedures in the library are:
;
;    High-level
;      CMSAVE - save variables to a save file
;      CMRESTORE - restore variables from a save file
;      CMSAVEDIR - list contents of a save file 
;      CMSVLIB (function) - this file
;
;    Mid-level  
;      CMSV_OPEN - open a save file for reading or writing
;      CMSVREAD - read non-pointer data from file
;      CMSVWRITE - write non-pointer data to file
;
;    Low-level
;      CMSV_RREC - read record from save file
;      CMSV_RVTYPE - read variable type information from file
;      CMSV_RDATA - read variable data from file
;      CMSV_WREC - write record to save file
;      CMSV_WVTYPE - write variable type information to file
;      CMSV_WDATA - write variable data to file
;
;    Utility
;      CMSV_RRAW (function) - read raw integer or string data from file
;      CMSV_WRAW - write raw integer or string data to file
;      CMSV_PTRSUM - create a heap data inventory
;      CMSV_TEST - test the library
;      TAGSIZE (function) - determine the types of all tags in a structure
;      HELPFORM (function) - create HELP-like string describing a variable
;
;   This procedure is part of the CMSVLIB SAVE library for IDL by
;   Craig Markwardt.  You must have the full CMSVLIB core package
;   installed in order for this procedure to function properly.  
;
;
; ==================================================================
;   Research Systems, Inc. has issued a separate license intended
;   to resolve any potential conflict between this software and the
;   IDL End User License Agreement. The text of that license
;   can be found in the file LICENSE.RSI, included with this
;   software library.
; ==================================================================
;
; INPUTS:
;
;   None
;
; KEYWORDS:
;
;   QUERY - if set, determine whether the CMSVLIB library is
;           installed.  Function returns 1 upon success, 0 upon
;           failure.
;
;   VERSION - upon return, the VERSION keyword will be set to a string
;             describing the version number of the CMSVLIB library.
;
; EXAMPLE:
;
;
; SEE ALSO:
;
;   CMRESTORE, SAVE, RESTORE, CMSVLIB
;
; MODIFICATION HISTORY:
;   Written, 2000
;   Documented, 24 Jan 2001
;   Added notification about RSI License, 13 May 2002, CM
;   Documented the VERSION keyword, 22 Nov 2009, CM
;
; LIBRARY MODIFICATIONS
;   1.0 - initial release
;   1.1 - 2003-06-28 - CMSV_RREC - added NOTICE record type
;   1.2 - 2006-03-07 - CMSV_RVTYPE - avoid reserved word INHERITS
;   1.3 - 2006-03-27 - CMSV_WDATA - add support to write bytes & empty
;         strings
;   1.4 - 2009-11-16 - CMSV_RREC - NEXTREC field is ULONG
;   1.5 - 2009-11-22 - CMSV_RDATA - clarify & speed some code
;   1.6 - 2010-01-11 - CMSV_RREC - read 64-bit files
;   1.7 - 2012-04-05 - CMSV_WRAW - writing strings >128 fixed
;   1.8 - 2013-04-18 - CMSV_WDATA - bug fix multi-dimensional byte array
    version = '1.8'  ;; NOTE: modify this when incrementing version number
;
; $Id: cmsvlib.pro,v 1.8 2013/04/18 19:14:09 cmarkwar Exp $
;
;-
; Copyright (C) 2000-2001, 2009, 2010, 2012, 2013, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-


  forward_function cmsv_rraw

  catch, catcherr
  if catcherr EQ 0 then cmsv_open, /query
  catch, /cancel
  if catcherr NE 0 then $
    message, 'ERROR: The complete CMSVLIB library must be in your IDL path.'

  return, 1
end
