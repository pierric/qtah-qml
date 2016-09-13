#ifndef __QmlProxyObject_H_
#define __QmlProxyObject_H_

#include <QObject>
#include <QString>
#include "qtahqml.h"


struct MetaStringData;

class QmlProxyObject: public QObject
{
public:
    QmlProxyObject(HsClassInfo*);
    Q_DISABLE_COPY(QmlProxyObject)
    virtual ~QmlProxyObject();
    virtual const QMetaObject* metaObject() const;
    virtual void* qt_metacast(const char*); 
    virtual int qt_metacall(QMetaObject::Call, int, void**);
private:
    static void qt_static_metacall(QObject *, QMetaObject::Call, int, void **);
    
    QScopedPointer<MetaStringData> metaStrings;
    QScopedPointer<uint> metaData;
    QMetaObject meta;
    
    QString className;
    int numMethods;
    HsUniformFun *ptrMethods;
};

#endif