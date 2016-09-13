#ifndef __TYPES_H_
#define __TYPES_H_

#ifdef __cplusplus
class QObject;
class QJSValue;
class QmlProxyObject;
extern "C" {
#else
typedef char QObject;
typedef char QJSValue;
typedef char QmlProxyObject;
#endif

typedef QJSValue* (*HsUniformFun)(const QJSValue*);

struct HsClassInfo
{
    char *className;
    int numMethods;
    HsUniformFun *ptrMethods;
    char **methodsName;
};

QmlProxyObject*genpop__QmlProxyObject_new(struct HsClassInfo*);
void gendel__QmlProxyObject(QmlProxyObject const*self);
QObject const*gencast__QmlProxyObject__QObject(QmlProxyObject const*self);
QmlProxyObject const*gencast__QObject__QmlProxyObject(QObject const*self);

void dump_QmlProxyObject(QmlProxyObject *self);

#ifdef __cplusplus
}
#endif


#endif