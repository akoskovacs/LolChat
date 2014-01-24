#-------------------------------------------------
#
# Project created by QtCreator 2014-01-10T16:40:15
#
#-------------------------------------------------

QT       += core gui network

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = ChatClient
TEMPLATE = app


SOURCES += main.cxx\
        ChatWindow.cxx \
    ConnectionDialog.cxx \
    ChatClient.cxx

HEADERS  += ChatWindow.hxx \
    ConnectionDialog.hxx \
    ChatClient.hxx

FORMS    += ChatWindow.ui \
    ConnectionDialog.ui
