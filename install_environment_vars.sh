

if [ `uname -m` == "x86_64" ]; then
  export ARBB_ARCH=intel64
else
  export ARBB_ARCH=ia32
fi
echo For ArBB purposes, using machine type: $ARBB_ARCH

# Here's a hackish way of finding the latest ArBB 
if [ -d /opt/intel/arbb/latest ]; then
  export ARBBD=/opt/intel/arbb/latest
else
  last=`ls /opt/intel/arbb/ | tail -n1`
  export ARBBD=/opt/intel/arbb/$last
fi
echo Found ArBB directory at: $ARBBD

# export C_INCLUDE_PATH=/opt/intel/arbb/latest/include/:$C_INCLUDE_PATH
#export C_INCLUDE_PATH=$ARBBD/include/:$C_INCLUDE_PATH

# RRN: I ran into trouble with C_INCLUDE_PATH and g++
export CPATH=$ARBBD/include/:$CPATH

export DYLD_LIBRARY_PATH=$ARBBD/lib/$ARBB_ARCH/:$DYLD_LIBRARY_PATH
export LD_LIBRARY_PATH=$ARBBD/lib/$ARBB_ARCH/:$LD_LIBRARY_PATH
export LIBRARY_PATH=$ARBBD/lib/$ARBB_ARCH/:$LIBRARY_PATH
