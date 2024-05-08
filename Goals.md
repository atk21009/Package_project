# Goals

### Tracking Project Design

1. Using the design components shown to you by your instructor, design the package tracking system that you will later build through the semester.

2. Using the provided learning assistant, each team member must have 3 Digital Ocean droplets created. You must be able to SSH into each droplet as a non-root user. That user must be able to use sudo.

### OTP Code Creation

1. Using the learning assistant provided, explore EUnit, unit testing, and mocking.

2. Create a GitHub repository called tracker_business_logic.

3. As a team and using the provided learning assistant and templates, create, as part of the tracker_business_logic repo, each gen_server, gen_statem, and gen_event your team designed. Write EUnit tests for these after you stub out the functions and before you write code for those functions.

4. Modify your modules until they all pass their unit tests.

### Web Service Creation

1. Assign a unique, appropriately named sub-domain to each of your droplets.

2. Using the learning assistant provided, setup your DMZ firewalls for your web service.

3. Create a rebar3 project on your web service server. Have the service use cowboy to handle web traffic. Store this project in a GitHub repository called tracker_service.

4. Create a cowboy handler to sent ‘hello world’ back out to your client.

5. Decide as a team which JSON library your project will use. In a junk rebar3 repo on a local machine, figure out how to use the library you’ve chosen.

6. Create a cowboy handler for each message type in the provided data protocol contract.

7. Use [Postman](https://www.postman.com/Links) to send JSON data to your web service in accordance with the provided data protocol contract. Use your library to decode the JSON text, and echo a unique, hardcoded response to each request.

### Full Data Flow

1. On one of the droplets behind your DMZ, install and run the [riak database](https://github.com/basho/riakLinks).

2. On the other droplet behind your DMZ, clone your tracker business logic and start it running.

3. Modify each of your cowboy handlers to use the appropriate OTP element you created for your tracker business logic.

4. Perform a Component level test to find and squash bugs. Do this until each type of message in the data protocol contract behaves correctly.

### Stress Testing Cycle

Using the stress tester provided,

1. begin finding unanticipated bugs in your code. Squash them.

2. Using the learning assistants provided, identify a bottleneck in your system.

3. Resolve the bottleneck

4. Repeat at step 2.

Continue this loop for the remainder of the semester to grow your system to handle as many processes as possible.
