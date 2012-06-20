#!/bin/sh

# Transform an existing saved state of a Curry program into
# a saved state that is executable independent of the
# generation environment, i.e.,
# - add path information to the saved state
# - add locale information (LANG, LC_ALL) to the saved state

# Compute the main directory where PAKCS is installed:
PAKCSHOME=`echo PAKCSHOME must be defined here!`
export PAKCSHOME

STANDALONE=no
if [ "$1" = "-standalone" ] ; then
  STANDALONE=yes
  shift
fi

ERROR=no
if [ "$1" = "-error" ] ; then
  ERROR=yes
  shift
fi

if [ $# != 1 ] ; then
  echo "Usage: $0 [-standalone|-error] <saved_state_file>"
  echo "-standalone: transform saved state into stand-alone executable"
  echo "-error     : do not suppress messages on standard error output"
  exit 1
fi

# Load definition of SPLD
. "$PAKCSHOME/bin/.pakcs_variables"

if [ "$PAKCSLIBPATH" = "" ] ; then
  PAKCSLIBPATH="$PAKCSHOME/lib:$PAKCSHOME/lib/meta"
  export PAKCSLIBPATH
fi

STATE=$1
TMPSTATE=$STATE$$
if test ! -f "$STATE" ; then
  echo "ERROR: saved state '$STATE' does not exist!"
  exit 1
fi

if [ $STANDALONE = yes ] ; then
  if [ -n "$SICSTUSDIR" ] ; then
    SPLD=$SICSTUSDIR/bin/spld
    if [ ! -x "$SPLD" ] ; then
      echo "ERROR: SICStus Prolog application builder '$SPLD' not found!" >&2
      exit 1
    fi
    mv "$STATE" main.sav
    "$SPLD" --static --main=restore --resources-from-sav --resources=main.sav=`pwd`/main.sav --output=$STATE
    rm -f main.sav
    chmod 755 "$STATE"
    echo "Stand-alone executable '$STATE' generated."
    exit 0
  else
    # we have SWI-Prolog and nothing must be done (everything was done in saveprog)
    exit 0
  fi
fi

# Patch the Sicstus saved state to suppress startup infos like version# 
# and add current local and PATH information:
mv $STATE $TMPSTATE
TMPFILE=TMPSAVEDSTATE$$
echo "#!/bin/sh" > $TMPFILE
if test -n "$LANG" ; then
  echo "LANG=$LANG" >> $TMPFILE
  echo "export LANG" >> $TMPFILE
fi
if test -n "$LC_ALL" ; then
  echo "LC_ALL=$LC_ALL" >> $TMPFILE
  echo "export LC_ALL" >> $TMPFILE
fi
echo "PAKCSHOME=\"$PAKCSHOME\"" >> $TMPFILE
echo "export PAKCSHOME" >> $TMPFILE
echo "PAKCSLIBPATH=\"$PAKCSLIBPATH\"" >> $TMPFILE
echo "export PAKCSLIBPATH" >> $TMPFILE
echo "PATH=\"$PATH:$PAKCSHOME/bin:\$PATH\"" >> $TMPFILE
echo "export PATH" >> $TMPFILE
if [ $ERROR = no ] ; then
  echo "exec 2> /dev/null" >> $TMPFILE
fi
cat $TMPFILE $TMPSTATE > $STATE
rm -f $TMPFILE $TMPSTATE
chmod 755 $STATE