

export ARBB_ARCH=intel64
# ARRB_ARCH=ia32
export ARBBD=/opt/intel/arbb/latest/ 

# export C_INCLUDE_PATH=/opt/intel/arbb/latest/include/:$C_INCLUDE_PATH
export C_INCLUDE_PATH=$ARBBD/include/:$C_INCLUDE_PATH
export LD_LIBRARY_PATH=$ARBBD/lib/$ARBB_ARCH/:$LD_LIBRARY_PATH
