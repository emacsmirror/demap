cmake_minimum_required(VERSION 3.11.0)
project(demap VERSION 0.1.1)

include(CTest)
enable_testing()

set(FILES demap.el demap-minimap.el demap--tools.el demap-track-w.el)


set(PKG-FILE ${PROJECT_NAME}-pkg.el)
set(FILE-NAME ${PROJECT_NAME}-${PROJECT_VERSION})

set(PKG-STRING "(define-package \"${PROJECT_NAME}\" \"${PROJECT_VERSION}\")")
file(WRITE ${CMAKE_BINARY_DIR}/${PKG-FILE} ${PKG-STRING})

add_custom_target(${FILE-NAME}.tar ALL
  COMMAND tar -cf ${FILE-NAME}.tar
      --transform='s,^,${FILE-NAME}/,'
      ${PKG-FILE}
      -C ${CMAKE_SOURCE_DIR}
      ${FILES}
  DEPENDS ${PKG-FILE} ${FILES}
  )

