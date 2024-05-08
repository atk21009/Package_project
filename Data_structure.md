### Package Transfer

- What to implement : update the location of the package with the given id
- Seems to be called whenever the package arrives at a transfer location
- Info : location_id (String), package_id (String),
- What to send back : 500 if error, 200 if success
- URL /package_transferred

### Delivered

- What to implement : update package status to delivered once delivery is confirmed
- Called when the package is officially delivered to the customer
- Info: package_id(String)
- What to send back: 200 on success 500 if error
- URL /delivered

### Request Location

- What to implement : Return the location based on package_id
- Give the location of the package to the customer so they can track it
- Info: package_id
- What to send back 200, 500, location_id / lat and long
- URL /location_request

### Location Update

- What to implement : update the current latitude and longitude of the package
- The current location of the package while in transit
- Info (lat and long)
- What to send back 200, 500
- URL /location_update

### Total Data

- Package id (String) :
  - The package itself, Specific to the delivery and used to specify what information we are talking about
- Location id (String) :
  - The current location of the package, For example, Salt Lake City Transfer Center or Boise Transfer Center
- Latitude (Float) :
  - The current Latitude of the package while out for delivery
- Longitude (Float) :
  - the current longitude of the package while out for delivery
- Status (Bool) :
  - The current status of the package. Delivered or not
