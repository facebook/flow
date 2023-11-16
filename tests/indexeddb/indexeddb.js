// @flow

const openRequest = (window.indexedDB: IDBFactory).open('dbName', 1);

openRequest.onsuccess = function (e) {
    const db = e.target.result as IDBDatabase;

    const storeName = 'storeName';
    const transaction = db.transaction(storeName, 'readonly', {durability: 'relaxed'});

    transaction.objectStore(storeName).getAll();
    transaction.objectStore(storeName).getAll('q');
    transaction.objectStore(storeName).getAll(undefined, 3);
    transaction.objectStore(storeName).getAll('q', 3);
}
