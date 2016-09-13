#include <stdio.h>
#include "qtahqml.h"
#include "QmlProxyObject.hpp"
#include <QMetaMethod>

extern "C" {
QmlProxyObject*genpop__QmlProxyObject_new(HsClassInfo *i)
{
    return new QmlProxyObject(i);
}
void gendel__QmlProxyObject(QmlProxyObject const*self)
{
    delete self;
}
QObject const*gencast__QmlProxyObject__QObject(QmlProxyObject const*self)
{
    return self;
}
QmlProxyObject const*gencast__QObject__QmlProxyObject(QObject const*self)
{
    return dynamic_cast<QmlProxyObject const*>(self);
}

void test(QmlProxyObject *self)
{
    printf(
        "[test]\n"
        "ptr: %p\n"
        , self);
    const QMetaObject *meta = self->metaObject();
    printf(
        "meta: %p\n"
        "  className: %s\n"
        "  constructorCount: %d\n" 
        "  methodCount: %d\n"
        "  methodOffset: %d\n"
        "  propertyCount: %d\n"
        "  propertyOffset: %d\n"
        , meta
        , meta->className()
        , meta->constructorCount()
        , meta->methodCount()
        , meta->methodOffset()
        , meta->propertyCount()
        , meta->propertyOffset());
    printf(
        "indexOf f1: %d\n"
        "indexOf f2: %d\n"
        , meta->indexOfSlot("f1(QJSValue)")
        , meta->indexOfSlot("f2(QJSValue)"));
    QMetaMethod f1 = meta->method(5);
    printf(
        "f1 info\n"
        "  signatre: %s\n"
        "  type: %d\n"
        , f1.methodSignature().data()
        , f1.methodType());
}
}