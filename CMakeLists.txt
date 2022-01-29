cmake_minimum_required(VERSION 3.11.0)
project(demap VERSION 0.3.0)

include(CTest)
enable_testing()

set(FILES demap-pkg.el demap.el demap-minimap.el demap--tools.el demap-track-w.el)


set(FILE-NAME ${PROJECT_NAME}-${PROJECT_VERSION})

add_custom_target(${FILE-NAME}.tar ALL
  COMMAND tar -cf ${FILE-NAME}.tar
      --transform='s,^,${FILE-NAME}/,'
      -C ${CMAKE_SOURCE_DIR}
      ${FILES}
  DEPENDS ${FILES}
  )

