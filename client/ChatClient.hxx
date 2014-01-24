#ifndef CHATCLIENT_HXX
#define CHATCLIENT_HXX

#include <QTcpSocket>
#include <QList>

struct UserData;
struct ServerData;

class ChatClient : public QTcpSocket
{
    Q_OBJECT

public:
    explicit ChatClient(QObject *parent = 0);
    void connectToHost(const ServerData *);
    inline QString getServerType() const {
        return m_serverType;
    }

    inline QString getServerVersion() const {
        return m_serverVersion;
    }

    void setTimeout(unsigned int);
    /* AUTH <name> <password> */
    void authenticate(UserData *);
    /* MSG <message> */
    void sendMessage(const QString &);
    void readyForMessages();
    /* LOGOUT */
    void close();
    /* AWAY */
    void away(bool);

signals:
    /* MSG <name> <message> */
    void gotMessage(const QString &from, const QString &msg);
    void messageSent(); // MSG_SENT
    void badServer();
    void authFail();    // U_SHALL_NOT_PASS
    void authOk();      // U_IN
    void timeout();
    /* PEERS <[*]name_1> <[*]name_2> ... <[*]name_n> */
    void gotPeerList(const QStringList &);
    /* LOGIN <name> */
    void peerLogin(const QString &);
    /* LOGOUT <name> */
    void peerLogout(const QString &);
    /* AWAY <name> */
    void peerAway(const QString &);
    /* WAKE <name> */
    void peerWakeUp(const QString &);

private slots:
    void on_readyRead();

private:
    UserData *m_userData;
    QString   m_serverType;
    QString   m_serverVersion;
};

#endif // CHATCLIENT_HXX
