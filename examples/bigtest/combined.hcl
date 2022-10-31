data "consul_service_health" "es_health" {
    count = 1
    
    depends_on = [     resource.nomad_job.elasticsearch ]
    
    derp = 1
    
    name = "es-transport-2.0"
    
    passing = true
    
    wait_for = "300s"
}

data "consul_service_health" "kibana_health" {
    count = 1
    
    depends_on = [     resource.nomad_job.kibana ]
    
    name = "kibana"
    
    passing = true
    
    wait_for = "300s"
}

resource "nomad_job" "elasticsearch" {
    count = 1
    
    jobspec = "job \"elasticsearch\" {\n  namespace = \"{default: , type: string, description: Job namespace}\"\n  all_at_once = false\n\n  constraint {\n    attribute = \"${attr.kernel.name}\"\n    operator = \"=\"\n    value = \"linux\"\n  }\n\n  constraint {\n    operator = \"distinct_hosts\"\n    value = \"true\"\n  }\n\n  datacenters = [\"{default: , type: string, description: Name of the datacenter for this nomad job}\"]\n\n\n  group \"elasticsearch-0\" {\n    count = 1\n\n    volume \"elasticsearch_data\" {\n      type = \"host\"\n      source = \"elasticsearch_data\"\n      read_only = false\n    }\n\n    task \"es-generic-node\" {\n\n      volume_mount {\n        volume = \"elasticsearch_data\"\n        destination = \"/var/lib/elasticsearch\"\n        read_only = false\n      }\n\n      config {\n        image = \"elasticsearch:7.3.2\"\n        // we need to set the network.host because otherwise, elasticsearch will bind to (and advertise) the docker container IP\n        // we need to set transport.port because otherwise, all the elasticsearch nodes will bind to and advertise 9300,\n        // and they won't like that they're all advertising the same address\n        // we need to set the host.name because it defaults to the docker container's hostname\n        args = [\"-c\",\"ln -s /local/unicast_hosts.txt /usr/share/elasticsearch/config/unicast_hosts.txt; elasticsearch -Ecluster.name=radix-es -Ediscovery.seed_providers=file -Enetwork.host=127.0.0.1 -Etransport.port=9300.0 -Enode.name=es0 -Ecluster.initial_master_nodes=es0,\"]\n        auth_soft_fail = false\n        command = \"bash\"\n        ulimit = {\n          \"nofile\" = \"65536\",\n          \"nproc\" = \"8192\",\n          \"memlock\" = \"-1\",\n        }\n        privileged = false\n        volumes = [\"/opt/radix/timberland:/timberland\"]\n        cap_add = []\n      }\n\n      driver = \"docker\"\n      env = {\n        \"ES_JAVA_OPTS\" = \"-Xms8g -Xmx8g\"\n      }\n      kill_timeout = \"5s\"\n      kill_signal = \"SIGINT\"\n      leader = false\n\n      resources {\n        memory = 10000\n        memory_max = 15000\n      }\n\n      shutdown_delay = \"0s\"\n      user = \"elasticsearch\"\n\n      template {\n        change_mode = \"noop\"\n        destination = \"local/unicast_hosts.txt\"\n        env = false\n        left_delimiter = \"{{\"\n        perms = \"644\"\n        right_delimiter = \"}}\"\n        data = <<EOH\n127.0.0.1:9300.0\n\n\n\nEOH\n        splay = \"5s\"\n      }\n    }\n\n    network {\n      mbits = 10\n      mode = \"bridge\"\n    }\n\n    // we need to increment the port to match the network.port in the args\n    service {\n      name = \"es-transport-0\"\n      port = 9300.0\n      tags = [\"elasticsearch\",\"transport\", \"system\"]\n      address_mode = \"auto\"\n\n      connect {\n        sidecar_service {\n          proxy {\n\n\n\n            upstreams {\n              destination_name = \"es-transport-0\"\n              local_bind_port = 9300.0\n            }\n\n\n          }\n        }\n      }\n\n      check {\n        type = \"script\"\n        name = \"es-transport-health-0\"\n        task = \"es-generic-node\"\n        command = \"curl\"\n        args = [\"127.0.0.1:9300.0\"]\n        interval = \"30s\"\n        timeout = \"5s\"\n      }\n    }\n\n    // we don't need to change the port of the rest service because elasticsearch doesn't use it to cluster\n    service {\n      name = \"es-rest-0\"\n      port = 9200\n      tags = [\"elasticsearch\",\"rest\", \"system\"]\n      address_mode = \"auto\"\n\n      connect {\n        sidecar_service {\n        }\n      }\n\n      check {\n        type = \"script\"\n        name = \"es-rest-health-0\"\n        task = \"es-generic-node\"\n        command = \"curl\"\n        args = [\"127.0.0.1:9200\"]\n        interval = \"30s\"\n        timeout = \"5s\"\n      }\n    }\n  }\n\n\n  priority = 50\n  region = \"global\"\n  type = \"service\"\n\n  update {\n    max_parallel = 1\n    health_check = \"checks\"\n    min_healthy_time = \"10s\"\n    healthy_deadline = \"5m\"\n    progress_deadline = \"10m\"\n    auto_revert = false\n    canary = 0\n    stagger = \"10s\"\n  }\n\n}\n"
}

resource "nomad_job" "kibana" {
    count = 1
    
    jobspec = "job \"kibana\" {\n  namespace = \"{default: , type: string, description: Job namespace}\"\n  all_at_once = false\n\n  constraint {\n    attribute = \"${attr.kernel.name}\"\n    operator = \"=\"\n    value = \"linux\"\n  }\n\n  constraint {\n    operator = \"distinct_hosts\"\n    value = \"true\"\n  }\n\n  datacenters = [\"{default: , type: string, description: Name of the datacenter for this nomad job}\"]\n\n  group \"kibana\" {\n\n    //kibana does not replicate\n    count = 1\n\n    task \"kibana\" {\n\n      config {\n        image = \"registry.gitlab.com/radix-labs/kibana-gantt\"\n        // it wants the elasticsearch hosts in json format: [\"host1\",\"host2\",...]\n        args = [\"--elasticsearch.hosts=[\\\"http://${NOMAD_UPSTREAM_ADDR_es-rest-0}\\\",]\",\"--server.host=0.0.0.0\",\"--path.data=/alloc/data\",\"--elasticsearch.preserveHost=false\",\"--xpack.apm.ui.enabled=false\",\"--xpack.graph.enabled=false\",\"--xpack.ml.enabled=false\"]\n        auth_soft_fail = false\n        command = \"kibana\"\n        ulimit = {\n          \"nofile\" = \"65536\",\n          \"nproc\" = \"8192\",\n          \"memlock\" = \"-1\"\n        }\n        privileged = false\n        cap_add = []\n      }\n\n      driver = \"docker\"\n      env = {\n        \"NODE_OPTIONS\" = \"--max-old-space-size=1024\"\n      }\n      kill_timeout = \"5s\"\n      kill_signal = \"SIGINT\"\n      leader = false\n\n      resources {\n        cpu = 1024\n        memory = 4096\n        memory_max = 6144\n      }\n\n      shutdown_delay = \"0s\"\n    }\n\n    network {\n      mbits = 10\n      mode = \"bridge\"\n      port \"kibanax\" {\n        to = 5601\n        static = 5601\n      }\n    }\n\n    service {\n      name = \"kibana\"\n      port = \"kibanax\"\n      tags = [\"kibana\",\"http\", \"user\"]\n      address_mode = \"auto\"\n\n      connect {\n        sidecar_service {\n          proxy {\n          \n            upstreams {\n              destination_name = \"es-rest-0\"\n              local_bind_port = 9200.0\n            }\n          \n          }\n        }\n      }\n    }\n  }\n\n  priority = 50\n  region = \"global\"\n  type = \"service\"\n\n  update {\n    max_parallel = 1\n    health_check = \"checks\"\n    min_healthy_time = \"10s\"\n    healthy_deadline = \"5m\"\n    progress_deadline = \"10m\"\n    auto_revert = false\n    canary = 0\n    stagger = \"10s\"\n  }\n\n}\n"
}

