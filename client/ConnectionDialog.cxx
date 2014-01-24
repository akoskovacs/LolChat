#include "ConnectionDialog.hxx"
#include "ChatWindow.hxx"
#include "ui_ConnectionDialog.h"
#include <QIntValidator>
#include <QDebug>
#include <QUrl>
#include <QCloseEvent>

ConnectionDialog::ConnectionDialog(ChatWindow *parent
                                   , UserData *udat, ServerData *sdat) :
    QDialog(parent),
    ui(new Ui::ConnectionDialog), mainWindow(parent), userData(udat), serverData(sdat)
{
    ui->setupUi(this);
    updateUserData();
    updateServerData();
    ui->portEdit->setValidator(new QIntValidator(0, 65535, this));
    setWindowTitle(tr("Új kapcsolat"));
    ui->userNameEdit->setFocus();
}

ConnectionDialog::~ConnectionDialog()
{
    delete ui;
}

void ConnectionDialog::reject()
{
    qDebug() << "rejected";
    mainWindow->closeConnectionDialog();
    QDialog::reject();
}

void ConnectionDialog::accept()
{
    qDebug() << "accepted";
    if (serverData != 0) {
        if (ui->serverNameEdit->text().isEmpty())
            serverData->serverName = ui->serverNameEdit->placeholderText();
        else
            serverData->serverName = ui->serverNameEdit->text();

        if (ui->portEdit->text().isEmpty())
            serverData->serverPort = ui->portEdit->placeholderText().toUInt();
        else
            serverData->serverPort = ui->portEdit->text().toInt();
        mainWindow->saveServerData();
    }

    if (userData != 0) {
        userData->userName = ui->userNameEdit->text();
        userData->password = ui->passwordEdit->text();
        userData->rememberMe = ui->rememberMeBox->isChecked();
        mainWindow->saveUserData();
    }
    mainWindow->closeConnectionDialog();
    QDialog::accept();
}

void ConnectionDialog::updateServerData()
{
    ui->serverNameEdit->setText(serverData->serverName);
    ui->portEdit->setText(QString::number(serverData->serverPort));
}

void ConnectionDialog::updateUserData()
{
    ui->userNameEdit->setText(userData->userName);
    ui->passwordEdit->setText(userData->password);
    ui->rememberMeBox->setChecked(userData->rememberMe);
}
