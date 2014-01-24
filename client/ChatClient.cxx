#include "ChatClient.hxx"

#include "ChatWindow.hxx"

ChatClient::ChatClient(QObject *parent) :
    QTcpSocket(parent), m_userData(0)
{}

void ChatClient::connectToHost(const ServerData *srvData)
{
    QTcpSocket::connectToHost(srvData->serverName, srvData->serverPort);
}

void ChatClient::authenticate(UserData *udata)
{
    QTextStream stream(this);
    stream.setCodec("UTF-8");
    waitForReadyRead();
    QString line = stream.readLine();
    QStringList toks = line.split(" ");
    if (toks.count() != 4 || toks[0] != "HAI" || toks[1] != "ME") {
        emit badServer();
        return;
    } else {
        m_serverType    = toks[2];
        m_serverVersion = toks[3];
    }

    // Sponsored by NSA(C)(Tm)(R)
    stream << QString("AUTH %1 %2\n").arg(udata->userName)
              .arg(udata->password);
    stream.flush();
    waitForReadyRead();
    QString answer = stream.readLine();

    qDebug() << answer;
    if (answer.trimmed() == "U_IN") {
        m_userData = udata;
        emit authOk();
    } else {
        emit authFail();
    }
 }

void ChatClient::sendMessage(const QString &msg)
{
    QTextStream stream(this);
    stream.setCodec("UTF-8");
    stream << "MSG " << msg << '\n';
}

void ChatClient::readyForMessages()
{
    connect(this, SIGNAL(readyRead()), this, SLOT(on_readyRead()));
}

void ChatClient::close()
{
    disconnect(this, SIGNAL(readyRead()), this, SLOT(on_readyRead()));
    QTextStream stream(this);
    stream.setCodec("UTF-8");
    stream << "LOGOUT\n";
    QTcpSocket::close();
}

void ChatClient::on_readyRead()
{
    QTextStream stream(this);
    stream.setCodec("UTF-8");
    while (canReadLine()) {
    QString line = stream.readLine();
    qDebug() << "Got" << line;
    QStringList toks = line.split(" ");
    if (toks[0] == "MSG") {
        toks.takeAt(0);
        QString from = toks.takeAt(0);
        emit gotMessage(from, toks.join(" "));
    } else if (toks[0] == "PEERS") {
            toks.takeAt(0);
            emit gotPeerList(toks);
    } else if (toks[0] == "MSG_SENT") {
        emit messageSent();
    } else if (toks[0] == "LOGIN") {
        emit peerLogin(toks[1]);
    } else if (toks[0] == "LOGOUT") {
        emit peerLogout(toks[1]);
    } else if (toks[0] == "AWAY") {
        emit peerAway(toks[1]);
    } else if (toks[0] == "WAKE") {
        emit peerWakeUp(toks[1]);
    }
    }
}

void ChatClient::away(bool away)
{
    m_userData->isAway = away;
    QTextStream stream(this);
    stream.setCodec("UTF-8");
    if (away) {
        stream << "AWAY\n";
    } else {
        stream << "WAKE_UP\n";
    }
}
