
block "header" {
  title = "Jake Trimble"

  items = {
    "E-mail": "jatrimble777@gmail.com",
    "Phone": "+1 (805) 618-8835",
    "LinkedIn": "jatrimble777",
    "Github": "jtrim777-dev"
  }
}

block "sidebar" {
  section {
    title = "Education"

    items = [
      "**Northeastern University**, Boston, MA",
      "Bachelor of Science in Computer Science, Magna cum Laude, 2023",
      "Master of Science in Computer Science, exp. 2024"
    ]
  }

  section {
    title = "Skills"

    subsection {
      title = "Languages"

      items = [
        "*[fluent]* Scala, Java, Python, Shell",
        "*[proficient]* C, Typescript, SQL, Swift",
        "*[familiar]* OCaml, Lisp, x86"
      ]
    }

    subsection {
      title = "Frameworks & Systems"

      text = "Linux, Terraform, HashiStack, Akka, Kafka, nginx, Docker, AWS, Cats, Shapeless, Github Actions, Wireshark"
    }

    subsection {
      title = "Tools & Methods"

      text = "Git, Jira, Agile Workflow, Actor-based systems, µservices"
    }
  }

  section {
    title = "Coursework"

    items = [
      "Algorithms (Graduate)",
      "Computer Systems (Graduate)",
      "Programming Languages (Graduate)",
      "Compilers",
      "Networks & Systems",
      "Network Security",
      "Robotic Science & Systems",
      "Logic & Theory of Computation",
      "Object Oriented Design"
    ]
  }

  section {
    title = "Projects"

    subsection {
      title = "Artemis"

      text = "A multi-platform, hierarchical compiler for a custom language, implemented in Scala."
    }

    subsection {
      title = "hcl4s"

      text = "A Scala library for parsing Hashicorp Configuration Language."
    }
  }
}

block "main" {
  title = "Technical Experience"

  section {
    title = "Platform Engineer"

    location = "Metropolis Technologies, Seattle, WA"

    dates = {
      start: "July, 2022",
      end: "Dec. 2022"
    }

    items = [
      "Designed and maintained core system architectural components for the backend of a full-scale web application as part of the Platform Engineering team. Collaborated with Product Engineering to train developers in best practices",
      "Overhauled the CI/CD infrastructure for Metropolis’ primary application and its supporting tools. Facilitated the transition from AWS Elastic Beanstalk to AWS ECS",
      "Optimized build tooling via Docker and SBT configuration improvements leading to a 55% reduction in developer compile time"
    ]
  }

  section {
    title = "Software Engineer"

    location = "Radix Labs, Boston, MA"

    dates = {
      start: "Jan. 2021",
      end: "July, 2022"
    }

    items = [
      "Developed, deployed, and debugged Radix’s product and customer specific software on customer-facing timelines for clients from Fortune 500 orgs to startups. Engineered around yet-unimplemented core product functionality and significant bugs",
      "Developed software interfaces for high-value biotech equipment from potentially erroneous documentation. Reverse-engineered communications protocols including RS232, USB, OPC, COM, MODBUS and REST",
      "Integrated drivers with Radix’s distributed runtime system and implemented data streaming in Kafka. Debugged complicated interprocess and network system bugs independently and in collaboration with other team-members",
      "Managed an agile team of co-op and full-time engineers doing driver and client work. Supported growth hiring by leading phone screens and technical interviews",
      {
        text = "Participate in daily updates and biweekly sprint plannings to facilitate integration across all facets of the team and perform iterative improvements of the development process",
        skip = true
      }
    ]
  }

  section {
    title = "Control Systems Developer"

    location = "NASA Big Idea"

    dates = {
      start: "March, 2020",
      end: "Dec. 2020"
    }

    items = [
      "Designed, implemented, and documented IR communications software in Python and C++ for use in a moon rover system per NASA specifications",
      "Integrated software in collaboration with multi-disciplinary team of Controls, Electrical, and Mechanical engineers"
    ]
  }

  section {
    title = "Mobile and Web Developer"

    location = "Hello World, Santa Barbara, CA"

    dates = {
      start: "April, 2019",
      end: "Jan. 2020"
    }

    items = [
      "Developed a Swift mobile application and Python website to facilitate field data recording during shoreline topography surveys",
      "Trained new developers to support code base development and management using version control (Git), IDE, and language-specific technologies, specifically including Cocoapods, Flask, and Firebase"
    ]
  }

  section {
    title = "Research Assistant"

    location = "UCSB, Santa Barbara, CA"

    dates = "Summer 2018"

    items = [
      "Developed algorithms in Python to retrieve, clean, analyze, and interpret raw genetic data from metagenomic sequencing into bins with a non-software-technical doctoral student",
      "Authored & presented a professional-standard research paper, presentation, and poster entitled \"The Characterization of Uncultured Microbes though the Analysis of Metagenomic Data\""
    ]
  }
}