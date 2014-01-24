#ifndef CONNECTIONDIALOG_HXX
#define CONNECTIONDIALOG_HXX

#include <QDialog>

namespace Ui {
class ConnectionDialog;
}

class ChatWindow;
struct UserData;
struct ServerData;

class ConnectionDialog : public QDialog
{
    Q_OBJECT
    
public:
    explicit ConnectionDialog(ChatWindow *parent, UserData *, ServerData *);
    ~ConnectionDialog();

private slots:
    void reject();
    void accept();

private:
    void updateUserData();
    void updateServerData();

    Ui::ConnectionDialog *ui;
    ChatWindow *mainWindow;
    UserData   *userData;
    ServerData *serverData;
};

#endif // CONNECTIONDIALOG_HXX
