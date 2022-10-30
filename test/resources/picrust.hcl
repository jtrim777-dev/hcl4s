
image "alpha" {
    partition "boot" {
      from = "https://maven.jtrim777.dev/releases/dev/jtrim777/raspian/0.0.1/boot.tgz"
      size = "365M"
      format = "fat32"
    }

    partition "rootfs" {
      from = "https://maven.jtrim777.dev/releases/dev/jtrim777/raspian/0.0.1/rootfs.tgz"
      size = "4G"
      format = "fat32"

      mounts = {
        "rootfs/homedir" = "/home/pi",
        "rootfs/locales.conf" = "/etc/defaults/locale",
        "rootfs/locales.gen" = "/etc/locale.gen"
      }
    }

    partition "vault" {
      fill = true
      format = "fat32"

      ablock "label" {
        foo = "bar"
      }
    }

    config {
        network {
            ssid = "Iris223"
            password = env.WIFI_PASSWORD
        }

        ssh {
            enabled = true
            allow_pass = false
        }

        locale {
            timezone = "America/Los_Angeles"
            locale = "en_US.UTF-8"
        }
    }

    hook "first-boot" {

    }

    hook "first-boot" "on-network" {

    }
}
