new Error('extra details for the root cause', {cause: new Error('root cause')});

new Error('extra details for the root cause', {cause: 'root cause'});
