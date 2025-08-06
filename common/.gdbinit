define attach-maya
python import gdb, subprocess
python gdb.execute('attach %s' % subprocess.check_output(["pidof", "maya.bin"]).split()[0].decode("utf-8"))
end

define attach-houdini
python import gdb, subprocess
python gdb.execute('attach %s' % subprocess.check_output(["pidof", "houdini-bin"]).split()[0].decode("utf-8"))
end

set print pretty on
set history save on
set history filename ~/.gdb_history

# emacs gets stuck asking for this
# set debuginfod enabled off
# gdb complaining, unsetting DEBUGINFOD_URLS instead


