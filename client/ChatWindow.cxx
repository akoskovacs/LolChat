#include "ChatWindow.hxx"
#include "ui_ChatWindow.h"

#include "ConnectionDialog.hxx"
#include "ChatClient.hxx"

#include <QCloseEvent>
#include <QDebug>
#include <QMessageBox>

namespace {
    void clearListWidget(QListWidget *w)
    {
        while (w->count() > 0) {
            delete w->takeItem(0);
        }
    }
}

ChatWindow::ChatWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::ChatWindow)
{
    ui->setupUi(this);
    m_userData   = 0;
    m_serverData = 0;
    m_connDialog = 0;

    m_chatClient = new ChatClient(this);
    makeConnections();
    //connect(m_chatClient, SIGNAL())

    setWindowTitle(tr("ChitChat v0.1"));
    ui->statusBar->showMessage(tr("Nincs kapcsolat..."));
#if 0
    ui->splitter->setStretchFactor(0, 0);
    ui->splitter->setStretchFactor(1, 1);
#endif
    readConfiguration();
    disableUI();
    ui->messageEdit->setFocus();
    emit on_newConnectionAction_triggered();
}

void ChatWindow::makeConnections()
{
    connect(m_chatClient, SIGNAL(connected()), this, SLOT(on_connected()));
    connect(m_chatClient, SIGNAL(authOk()), this, SLOT(on_authOk()));
    connect(m_chatClient, SIGNAL(authFail()), this, SLOT(on_authFail()));
    connect(m_chatClient, SIGNAL(disconnected()), this, SLOT(on_disconnected()));
    connect(m_chatClient, SIGNAL(gotPeerList(QStringList)), this, SLOT(on_gotPeerList(QStringList)));
    connect(m_chatClient, SIGNAL(gotMessage(QString,QString)), this, SLOT(on_gotMessage(QString,QString)));
    connect(m_chatClient, SIGNAL(messageSent()), this, SLOT(on_messageSent()));
    connect(m_chatClient, SIGNAL(peerLogin(QString)), this, SLOT(on_peerLogin(QString)));
    connect(m_chatClient, SIGNAL(peerLogout(QString)), this, SLOT(on_peerLogout(QString)));
    connect(m_chatClient, SIGNAL(peerAway(QString)), this, SLOT(on_peerAway(QString)));
    connect(m_chatClient, SIGNAL(peerWakeUp(QString)), this, SLOT(on_peerWakeUp(QString)));
    connect(m_chatClient, SIGNAL(badServer()), this, SLOT(on_badServer()));
    connect(m_chatClient, SIGNAL(error(QAbstractSocket::SocketError)), this
            , SLOT(on_socketError(QAbstractSocket::SocketError)));
}

ChatWindow::~ChatWindow()
{
    delete m_userData;
    delete m_serverData;
    delete m_chatClient;
    delete ui;
}

void ChatWindow::on_socketError(QAbstractSocket::SocketError _error)
{
    QMessageBox::warning(this, tr("Hiba a kapcsolatban")
                         , m_chatClient->errorString());
}

void ChatWindow::disableUI()
{
    clearListWidget(ui->peerListWidget);
    ui->peerListWidget->setEnabled(false);
    ui->peerMessagesEdit->setEnabled(false);
    ui->messageEdit->setEnabled(false);
    ui->sendButton->setEnabled(false);
    ui->peerCountLabel->setText(tr("0 felhasználó"));
    ui->userNameLabel->setText("");
    ui->messageEdit->clear();
    ui->peerMessagesEdit->clear();
    ui->connectionDetailAction->setEnabled(false);
}

void ChatWindow::saveUserData()
{
    m_appSettings.beginGroup("user");
    if (m_userData->rememberMe) {
        m_appSettings.setValue("remember_me", true);
        m_appSettings.setValue("user_name", m_userData->userName);
        // XXX: Secret NSA backdoor, _DO_NOT_CHANGE_
        m_appSettings.setValue("user_password", m_userData->password);
    } else {
        m_appSettings.setValue("remember_me", false);
        m_appSettings.setValue("user_name", "");
        m_appSettings.setValue("user_password", "");
    }
    m_appSettings.endGroup();
}

void ChatWindow::saveServerData()
{
    QString urlString = QString("%1:%2").arg(m_serverData->serverName)
            .arg(m_serverData->serverPort);
#if 0
    QUrl url(urlString);
    if (!url.isValid()) {
        QMessageBox::warning(this, tr("Rosz gépcím!")
                             , tr("A gépnév formátuma nem megfelelő: '%1'").arg(urlString));
        emit on_newConnectionAction_triggered();
    }
#endif
    m_appSettings.beginGroup("network");
    m_appSettings.setValue("server_name", m_serverData->serverName);
    m_appSettings.setValue("server_port", m_serverData->serverPort);
    m_appSettings.endGroup();
}

void ChatWindow::closeConnectionDialog()
{
    qDebug() << "Close dialog";
    m_connDialog = 0;
}

void ChatWindow::on_newConnectionAction_triggered()
{
    if (m_connDialog != 0)
        return;
    qDebug() << "Open connection dialog";
    closeConnections();
    m_connDialog = new ConnectionDialog(this, m_userData, m_serverData);
    connect(m_connDialog, SIGNAL(accepted()), this, SLOT(on_connectionDialogAccepted()));
    m_connDialog->setWindowState(Qt::WindowActive);
    m_connDialog->show();
}

void ChatWindow::on_newWindowAction_triggered()
{
    (new ChatWindow())->show();
}

void ChatWindow::on_connectionDialogAccepted()
{
    m_chatClient->connectToHost(m_serverData);
}

void ChatWindow::closeConnections()
{
    if (m_chatClient->isOpen()) {
        ui->statusBar->showMessage(tr("Nyitott kapcsolatok lezárása..."), 1000);
        m_chatClient->close();
        ui->closeConnectionAction->setEnabled(false);
        ui->statusBar->showMessage(tr("Kapcsolat bontva"), 1000);
    }
}

void ChatWindow::on_quitAction_triggered()
{
    close();
}

void ChatWindow::on_connected()
{
    ui->statusBar->showMessage(tr("Kapcsolódva a %1-hoz.")
                               .arg(m_serverData->serverName), 1000);
    ui->statusBar->showMessage(tr("Azonosítás %1-ként...").arg(m_userData->userName), 2000);
    ui->closeConnectionAction->setEnabled(true);

    m_chatClient->authenticate(m_userData);
}

void ChatWindow::on_disconnected()
{
    closeConnections();
    ui->statusBar->showMessage(tr("A kapcsolat megszakadt..."));
    disableUI();
}

void ChatWindow::on_gotPeerList(const QStringList &peers)
{
    clearListWidget(ui->peerListWidget);
    QStringListIterator it(peers);
    while (it.hasNext()) {
        QString peer = it.next();
        QListWidgetItem *item = new QListWidgetItem(ui->peerListWidget);
        if (peer[0] == '*') {
            item->setForeground(QBrush(Qt::gray));
            peer.remove(0, 1);
        }
        item->setText(peer);
        ui->peerListWidget->addItem(item);
    }

    ui->peerListWidget->setEnabled(true);
    ui->peerListWidget->sortItems();
    updatePeerCount();
    ui->statusBar->showMessage(tr("Felhasználólista frissítve..."));
}

void ChatWindow::on_badServer()
{
    QMessageBox::warning(this, tr("Rossz protokoll"),
                         tr("A szerver nem a protokoll szerint válaszolt"));
    closeConnections();
}

void ChatWindow::grayOutUser(const QString &name, bool is_gray)
{
    QList<QListWidgetItem *> items = ui->peerListWidget->findItems(name, Qt::MatchExactly);
    QMutableListIterator<QListWidgetItem *> it(items);
    while (it.hasNext()) {
        QListWidgetItem *item = it.next();
        item->setForeground(QBrush(is_gray ? Qt::gray :Qt::black));
    }
}

void ChatWindow::on_peerAway(const QString &name)
{
    grayOutUser(name, true);
    ui->peerMessagesEdit->append(tr("<font color=\"gray\"><i>%1 távol van...</i></font>")
            .arg(name));
}

void ChatWindow::on_peerWakeUp(const QString &name)
{
    grayOutUser(name, false);
    ui->peerMessagesEdit->append(tr("<font color=\"gray\"><i>%1 elérhető...</i></font>")
            .arg(name));
}

void ChatWindow::on_gotMessage(const QString &from, const QString &msg)
{
    QTextEdit *editor = ui->peerMessagesEdit;
    if (msg.startsWith(m_userData->userName)) {
        editor->append(QString("<p><font color=\"blue\"><b>%1<b>:</font> <font color=\"red\"><b>%2</b></p>")
                       .arg(from).arg(msg.toHtmlEscaped()));
    } else {
        editor->append(QString("<p><font color=\"blue\"><b>%1<b>:</font> %2</p>")
                       .arg(from).arg(msg.toHtmlEscaped()));
    }
}

void ChatWindow::on_authOk()
{
    ui->statusBar->showMessage(tr("Sikeresen azonosítva..."));
    ui->userNameLabel->setText(m_userData->userName + ":");
    ui->messageEdit->setEnabled(true);
    ui->sendButton->setEnabled(true);
    ui->peerMessagesEdit->setEnabled(true);
    ui->awayAction->setEnabled(true);
    ui->connectionDetailAction->setEnabled(true);
    m_chatClient->readyForMessages();
}

void ChatWindow::on_authFail()
{
    QMessageBox::warning(this, tr("Hiba"), tr("Sikertelen bejelentkezés"));
    ui->statusBar->showMessage(tr("Az azonosítás sikertelen"));
    on_newConnectionAction_triggered();
}

void ChatWindow::readConfiguration()
{
    m_appSettings.beginGroup("geometry");
    restoreGeometry(m_appSettings.value("geometry").toByteArray());
    restoreState(m_appSettings.value("window_state").toByteArray());
    m_appSettings.endGroup();

    m_appSettings.beginGroup("user");
    m_userData = new UserData;
    if (m_appSettings.value("remember_me").toBool()) {
        m_userData->userName = m_appSettings.value("user_name").toString();
        // XXX: Secret NSA backdoor, _DO_NOT_CHANGE_
        m_userData->password = m_appSettings.value("user_password").toString();
        m_userData->rememberMe = true;
    }
    m_appSettings.endGroup();

    m_appSettings.beginGroup("network");
    m_serverData = new ServerData;
    m_serverData->serverName = m_appSettings.value("server_name").toString();
    m_serverData->serverPort = m_appSettings.value("server_port").toUInt();
    m_appSettings.endGroup();
}

void ChatWindow::writeConfiguration()
{
    m_appSettings.beginGroup("geometry");
    m_appSettings.setValue("geometry", saveGeometry());
    m_appSettings.setValue("window_state", saveState());
    m_appSettings.endGroup();
    saveUserData();
    saveServerData();
}

void ChatWindow::closeEvent(QCloseEvent *event)
{
    closeConnections();
    writeConfiguration();
    QMainWindow::closeEvent(event);
}

void ChatWindow::on_closeConnectionAction_triggered()
{
    closeConnections();
}

void ChatWindow::on_messageSent()
{
    QString msg = ui->messageEdit->text();
    QTextEdit *editor = ui->peerMessagesEdit;
    editor->append(QString("<p><font color=\"gray\"><b>%1</b></font>: %2</p>")
                   .arg(m_userData->userName).arg(msg.toHtmlEscaped()));
    ui->messageEdit->clear();
}

void ChatWindow::updatePeerCount()
{
    ui->peerCountLabel->setText(tr("%1 felhasználó").arg(ui->peerListWidget->count()));
}

void ChatWindow::on_peerLogin(const QString &name)
{
    ui->peerListWidget->addItem(new QListWidgetItem(name, ui->peerListWidget));
    ui->peerListWidget->sortItems();
    ui->peerMessagesEdit->append(tr("<p><font color\"gray\"><i>%1 bejelentkezett...</i></font></p>")
            .arg(name));
    updatePeerCount();
}

void ChatWindow::on_peerLogout(const QString &name)
{
    QList<QListWidgetItem *> items = ui->peerListWidget->findItems(name, Qt::MatchExactly);
    if (items.empty())
        return;

    QMutableListIterator<QListWidgetItem *> it(items);
    while (it.hasNext()) {
        QListWidgetItem *item = it.next();
        ui->peerListWidget->removeItemWidget(item);
        delete item;
    }

    ui->peerMessagesEdit->append(
                tr("<p><font color\"gray\"><i>%1 kijelentkezett...</i></font></p>")
                                  .arg(name));
    updatePeerCount();
}

void ChatWindow::on_sendButton_clicked()
{
    if (!ui->messageEdit->text().isEmpty())
        m_chatClient->sendMessage(ui->messageEdit->text());
    else
        ui->statusBar->showMessage(tr("Az üzenet üres, azért sem küldöm"));
}

void ChatWindow::on_peerListWidget_doubleClicked(const QModelIndex &index)
{
    QListWidgetItem *item = ui->peerListWidget->item(index.row());
    ui->messageEdit->setText(ui->messageEdit->text()+item->text()+": ");
    ui->messageEdit->setFocus();
}

void ChatWindow::on_awayAction_triggered(bool checked)
{
    ui->messageEdit->setEnabled(!checked);
    ui->sendButton->setEnabled(!checked);
    m_chatClient->away(checked);
    grayOutUser(m_userData->userName, checked);
}

void ChatWindow::on_connectionDetailAction_triggered()
{
    QMessageBox::information(this, tr("Kapcsolat adatok")
    , tr("<p>Protokol: <i>LCCSP (LolCatChat Secure Protcol)</i></p>"
       "<p>Szerver típus: <i>%1</i></p>"
       "<p>Szerver verzió: <i>%2</i><p>"
       "<p>Szerver cím: <i>%3</i></p>"
       "<p>Szerver port: <i>%4</i></p>"
       "<p>A kapcsolat <b>Plaintextben</b> titkosított</p>"
       "&copy; Kovács Ákos - 2014")
                             .arg(m_chatClient->getServerType())
                             .arg(m_chatClient->getServerVersion())
                             .arg(m_serverData->serverName)
                             .arg(m_serverData->serverPort));
}
