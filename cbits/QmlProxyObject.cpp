#include <stdio.h>
#include <QJSValue> 
#include "QmlProxyObject.hpp"

const int MAX_NUM_SLOTS  = 20;
const int MAX_LEN_STRING = 512;

struct MetaStringData {
    QArrayData data[MAX_NUM_SLOTS];
    char stringdata0[MAX_LEN_STRING];
};

struct MetaDataHeader_t {
    uint _0[4];
    uint method_num;
    uint method_i;
    uint _1[8];
};
struct MetaDataMethodDesc_t {
    uint name_i;
    uint argc;
    uint param_i;
    uint tag;
    uint flag;
};
struct MetaDataMethodSign_t {
    uint ret;
    uint p1;
    uint flag1;
};
// number of uint in MetaDataMethodSign_t
const int method_sign_width = sizeof(MetaDataMethodSign_t) / sizeof(uint);

static const MetaDataHeader_t metaDataHeader = {
       7,       // revision
       0,       // classname, index in strings table
       0,    0, // classinfo
       0,   14, // methods      (number of meth, index in data table)    
       0,    0, // properties   (number of prop, index in data table)
       0,    0, // enums/sets   (number of enum, index in data table)
       0,    0, // constructors (number of cons, index in data table)
       0,       // flags
       0,       // signalCount
};
static const MetaDataMethodDesc_t metaDataMethodI = {
       0,       // name index, index in strings table 
       1,       // number of parameters
       0,       // parameter, index in data table
       2, 0x0a
};
static const MetaDataMethodSign_t metaDataMethodS = {
    // QJSValue foo(const QJSValue&)
    0x80000000 | 1, 0x80000000 | 1, 2,
};

static void dumpMetaStrings(const MetaStringData *, int, int);
static void dumpMetaData(const uint *, int);

QmlProxyObject::QmlProxyObject(HsClassInfo *info)
    : metaStrings(new MetaStringData)
    , metaData(new uint[14 + (5+method_sign_width)*info->numMethods + 1])
{
    // printf(
    //     "QmlProxyObject: %p\n"
    //     "info: %p\n"
    //     "  className:   %s\n"
    //     "  numMethods:  %d\n"
    //     "  ptrMethods:  %p\n"
    //     "  methodsName: %p\n"
    //     , this, info, info->className, info->numMethods, info->ptrMethods, info->methodsName);
    char *cptr = metaStrings->stringdata0;
    int len = strlen(info->className);
    if (len+1 > MAX_LEN_STRING)
        len = MAX_LEN_STRING-1;
    strncpy(cptr, info->className, len);
    cptr[len] = 0;
    metaStrings->data[0].ref.atomic.store(-1);
    metaStrings->data[0].size = len;
    metaStrings->data[0].alloc = 0;
    metaStrings->data[0].capacityReserved = 0;
    metaStrings->data[0].offset = cptr - (char *)&metaStrings->data[0];
    cptr += len+1;    

    strcpy(cptr, "QJSValue");
    metaStrings->data[1].ref.atomic.store(-1);
    metaStrings->data[1].size = 8;
    metaStrings->data[1].alloc = 0;
    metaStrings->data[1].capacityReserved = 0;
    metaStrings->data[1].offset = cptr - (char *)&metaStrings->data[1];
    cptr += 9;    

    const int meth_name_off = 2;

    int num = info->numMethods;
    if (num > MAX_NUM_SLOTS)
        num = MAX_NUM_SLOTS;
    for (int i=0; i < num; i++) {
        int len = strlen(info->methodsName[i]);
        if (cptr - metaStrings->stringdata0 + len + 1 > MAX_LEN_STRING) {
            num = i;
            break;
        }
        strncpy(cptr, info->methodsName[i], len);
        cptr[len] = 0;
        metaStrings->data[i+meth_name_off].ref.atomic.store(-1);
        metaStrings->data[i+meth_name_off].size = len;
        metaStrings->data[i+meth_name_off].alloc = 0;
        metaStrings->data[i+meth_name_off].capacityReserved = 0;
        metaStrings->data[i+meth_name_off].offset = cptr - (char *)&metaStrings->data[i+meth_name_off];
        cptr += len+1;
    }
    //dumpMetaStrings(metaStrings.data(), num+meth_name_off, cptr - metaStrings->stringdata0);

    MetaDataHeader_t *header = (MetaDataHeader_t *)metaData.data();
    memcpy(header, &metaDataHeader, sizeof(MetaDataHeader_t));
    header->method_num = num;
    MetaDataMethodDesc_t *md = (MetaDataMethodDesc_t *)&header[1];
    for (int i=0; i<num; i++) {
        memcpy(&md[i], &metaDataMethodI, sizeof(MetaDataMethodDesc_t));
        md[i].name_i  = i + meth_name_off;
        md[i].param_i = 14 + 5 * num + method_sign_width * i;
    }
    MetaDataMethodSign_t *ms = (MetaDataMethodSign_t *)&md[num];
    for (int i=0; i<num; i++) {
        memcpy(&ms[i], &metaDataMethodS, sizeof(MetaDataMethodSign_t));
    }
    uint *eos = (uint *)&ms[num];
    eos[0] = 0;
    //dumpMetaData(metaData.data(), 14 + (5+method_sign_width)*num + 1);
    
    meta.d.superdata  = &QObject::staticMetaObject;
    meta.d.stringdata = metaStrings->data;
    meta.d.data       = metaData.data();
    meta.d.static_metacall = qt_static_metacall;
    meta.d.relatedMetaObjects = Q_NULLPTR;
    meta.d.extradata  = Q_NULLPTR;

    className  = info->className;
    numMethods = num;
    ptrMethods = info->ptrMethods;
}

QmlProxyObject::~QmlProxyObject() 
{
}

void QmlProxyObject::qt_static_metacall(QObject *_o, QMetaObject::Call _c, int _id, void **_a)
{
    // printf("qt_static_metacall(): %d %d\n", _c, _id);
    if (_c == QMetaObject::InvokeMetaMethod) {
        QmlProxyObject *_t = static_cast<QmlProxyObject *>(_o);
        Q_UNUSED(_t)
        if (_id >=0 && _id < _t->numMethods) {
            // printf("  ret: %p \n", _a[0]);
            // printf("  arg: %p \n", _a[1]);
            void *_r = _t->ptrMethods[_id](reinterpret_cast<const QJSValue(*)>(_a[1]));
            if (_a[0])
                *reinterpret_cast< QJSValue*>(_a[0]) = *reinterpret_cast< QJSValue*>(_r);
        }
    }
}

const QMetaObject *QmlProxyObject::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->dynamicMetaObject() : &meta;
}

void *QmlProxyObject::qt_metacast(const char *_clname)
{
    if (!_clname) return Q_NULLPTR;
    if (className == _clname)
        return static_cast<void*>(const_cast< QmlProxyObject*>(this));
    return QObject::qt_metacast(_clname);
}

int QmlProxyObject::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    // printf("qt_metacall(): %d %d\n", _c, _id);
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        if (_id < numMethods)
            qt_static_metacall(this, _c, _id, _a);
        _id -= numMethods;
    } else if (_c == QMetaObject::RegisterMethodArgumentMetaType) {
        if (_id < numMethods)
            *reinterpret_cast<int*>(_a[0]) = -1;
        _id -= numMethods;
    }
    return _id;
}

void dumpMetaStrings(const MetaStringData *data, int numD, int numC)
{
    printf("meta strings (%p, %d, %d)\n"
        , data, numD, numC);
    char buf[256];
    for (int i=0;i<numD;i++) {
        int offset = data->data[i].offset;
        int size   = data->data[i].size;
        strncpy(buf, ((char*)&data->data[i]) + offset, size);
        buf[size] = 0;
        printf("  [%03x,%02d]%s\n", offset, size, buf);
    }
}
void dumpMetaData(const uint *data, int num)
{
    printf("meta data (%p, %d)\n"
        , data, num);
    int col = 0;
    const int stride = 32;
    for (int i=0;i<num;i++) {
        if (col == 0) printf("  ");
        printf("%02x ", data[i]);
        col++;
        if (col >= stride) {
            printf("\n");
            col = 0;
        }
    }
    if (col < stride-1) printf("\n");
}