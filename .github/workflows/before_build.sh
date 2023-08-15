BUILDING_ARCH=$(echo $CIBW_BUILD | cut -d '-' -f -2)
echo "BUILD AND ARCH"
echo $CIBW_BUILD
echo $BUILDING_ARCH
printenv
if [ "$BUILDING_ARCH" == "x86_64" ]; then
    export CIBW_ENVIRONMENT_MACOS="F77=gfortran-11 F90=gfortran-11"
    echo $CIBW_ENVIRONMENT_MACOS
    echo "CIBW_ENVIRONMENT_MACOS=$CIBW_ENVIRONMENT_MACOS" >> $GITHUB_ENV
elif [ "$BUILDING_ARCH" == "arm64" ]; then
  curl -L -O https://github.com/isuruf/gcc/releases/download/gcc-10-arm-20210728/gfortran-darwin-arm64.tar.gz
  sudo mkdir -p /opt/
  sudo cp "gfortran-darwin-arm64.tar.gz" /opt/gfortran-darwin-arm64.tar.gz
  pushd /opt
        sudo tar -xvf gfortran-darwin-arm64.tar.gz
        sudo rm gfortran-darwin-arm64.tar.gz
  popd
  export FC_ARM64="$(find /opt/gfortran-darwin-arm64/bin -name "*-gfortran")"
  libgfortran="$(find /opt/gfortran-darwin-arm64/lib -name libgfortran.dylib)"
  libdir=$(dirname $libgfortran)
  export FC_ARM64_LDFLAGS="-L$libdir -Wl,-rpath,$libdir"
  export SDKROOT=$(xcrun -sdk macosx --show-sdk-path) 
  export CIBW_ENVIRONMENT_MACOS="FC=$FC_ARM64 F90=$FC_ARM64 F77=$FC_ARM64 LDFLAGS=\" -arch arm64 $FC_ARM64_LDFLAGS\" CFLAGS=\" -arch arm64\" CXXFLAGS=\" -arch arm64\" CPPFLAGS=\" -arch arm64\" _PYTHON_HOST_PLATFORM=macosx-11.0-arm64 ARCHFLAGS=\" -arch arm64\" FCFLAGS=\" -arch arm64\" CROSS_COMPILING=1 host_alias=aarch64-apple-darwin20.0.0  MACOSX_DEPLOYMENT_TARGET=11.0 SDKROOT=$SDKROOT"
  echo $CIBW_ENVIRONMENT_MACOS
  echo "CIBW_ENVIRONMENT_MACOS=$CIBW_ENVIRONMENT_MACOS" >> $GITHUB_ENV
fi