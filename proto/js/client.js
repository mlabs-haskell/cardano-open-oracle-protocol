const oracleTypes = require('./generated-pb/oracle_pb.js');
const oracleGrpc = require('./generated-pb/oracle_grpc_web_pb.js');
var google_protobuf_empty_pb = require('google-protobuf/google/protobuf/empty_pb.js');

var oracleService = new oracleGrpc.OracleClient('https://localhost:5081');

var metadata = {'custom-header-1': 'value1'};

oracleService.getCatalog(new google_protobuf_empty_pb.Empty(), metadata, function(err, response) {
    if (err) {
        console.log(err.code);
        console.log(err.message);
    } else {
        console.log(response.getMessage());
    }
});
