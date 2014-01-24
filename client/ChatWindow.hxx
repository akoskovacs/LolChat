#ifndef CHATWINDOW_HXX
#define CHATWINDOW_HXX

#include <QMainWindow>
#include <QSettings>
#include <QPointer>
#include <QAbstractSocket>

namespace Ui {
class ChatWindow;
}

struct UserData {
    UserData()
        : userName(""), password(""), rememberMe(true)
        , isAway(false) {}

    QString userName;
    QString password;
    bool    rememberMe;
    bool    isAway;
};

struct ServerData {
    QString serverName;
    unsigned int serverPort;
};

class ChatClient;
class ConnectionDialog;
class ChatWindow : public QMainWindow
{
    Q_OBJECT
    
public:
    explicit ChatWindow(QWidget *parent = 0);
    void saveUserData();
    void saveServerData();
    void closeConnectionDialog();
    ~ChatWindow();
    
private slots:
    void on_newConnectionAction_triggered();
    void on_newWindowAction_triggered();
    void on_quitAction_triggered();
    void on_connectionDialogAccepted();
    void on_connected();
    void on_authOk();
    void on_authFail();
    void on_disconnected();
    void on_gotPeerList(const QStringList &);
    void on_badServer();
    void on_peerAway(const QString &);
    void on_peerWakeUp(const QString &);
    void on_gotMessage(const QString &, const QString &);
    void on_closeConnectionAction_triggered();
    void on_sendButton_clicked();
    void on_messageSent();
    void on_peerLogin(const QString &);
    void on_peerLogout(const QString &);
    void on_socketError(QAbstractSocket::SocketError);
    void on_peerListWidget_doubleClicked(const QModelIndex &index);
    void on_awayAction_triggered(bool checked);
    void on_connectionDetailAction_triggered();

protected:
    void closeEvent(QCloseEvent *);

private:
    void readConfiguration();
    void writeConfiguration();
    void closeConnections();
    void disableUI();
    void updatePeerCount();
    void makeConnections();
    void grayOutUser(const QString &, bool);

    Ui::ChatWindow   *ui;
    UserData         *m_userData;
    ServerData       *m_serverData;
    ChatClient       *m_chatClient;
    ConnectionDialog *m_connDialog;
    QSettings         m_appSettings;
};

#endif // CHATWINDOW_HXX
