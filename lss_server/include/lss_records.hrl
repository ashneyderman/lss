-record(device_record, {cid,                 % this is the ID we generate on the server side
                        tid,                 % this is the ID agent assigns to itself
                        connect_time,
                        disconnect_time,
                        total_pings = 0,
                        last_ping_time,
                        total_data_bytes = 0,
                        last_update_time}).

