#include "ChatWindow.hxx"
#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    a.setOrganizationName("Kovács Ákos");
    a.setApplicationName("ChitChat");
    ChatWindow w;
    w.show();
    return a.exec();
}
